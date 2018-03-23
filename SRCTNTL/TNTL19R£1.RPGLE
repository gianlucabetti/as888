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
005600980323     D TTRN            S              7  0 DIM(3000)                            *NUMERO TRAINO
005700980323     D TDDE            S              8  0 DIM(3000)                            *DECORRENZA
005800980323     D TTFP            S              8  0 DIM(3000)                            *FILIALE PARTENZA
005900980323     D TGHA            S              5    DIM(3000)                            *GIORNO ARRIVO
006000980323     D TGHP            S              5    DIM(3000)                            *GIORNO PARTENZA
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
009900000000     D*---------------------
010000000000     D* DS TIPI TRAINO
010100000000     D*---------------------
010200000000     D DSTV          E DS
010300000000     D*---------------------
010400000000     D* DS TIPI AUOTMEZZO
010500000000     D*---------------------
010600000000     D DSTU          E DS
010700000000     D*---------------------
010800000000     D* DS CAUSALI CONTABILIZZAZIONE
010900000000     D*---------------------
011000000211     D ANGY4ZDS      E DS
011100010928      *--------------------
011200010928      * Ds - moneta di conto
011300010928      *--------------------
011400010928     d dged          e ds
011500010928     d tibs02ds      e ds
011600010928     d dscv          e ds
011700010928     d yeurcods      e ds
011800000000     D*---------------------
011900000000     D* DS CONTROLLO DATA (8)
012000000000     D*------------------
012100000000     D WLBDA8          DS                  INZ
012200000000     D  G08DAT                 1      8  0
012300000000     D  G08INV                 9     16  0
012400000000     D  G08ERR                17     17
012500000000     D  G08TGI                18     22  0
012600000000     D*------------------
012700000000     D* DESCRIZIONE OPERAZIONE IN TESTATA
012800000000     D*------------------
012900960118     D* COMMENTI
013000000000     D DES999          C                   CONST('DI CARTELLO')
013100000000     C*------------------------------------------------------------------------*
013200000000     C* MAIN LINE
013300000000     C*------------------------------------------------------------------------*
013400000000     C*
013500000000     C* STAMPA PARAMETRI LANCIO
013600000000     C                   EXSR      STAPAR
013700000000     C*
013800960115     C* POSIZIONAMENTO E PRIMA LETTURA TRAINI CARIATI IN MEMORIA
013900960115     C                   Z-ADD     1             T
014000000000     C*
014100000000     C* FINO A FINE TRAINI
014200960115DO  1C     TTRN(T)       DOWGT     *ZEROS
014300000000     C*
014400000000     C* OPERAZIONI PER NUOVO TRAINO
014500000000     C                   EXSR      NEWTLT
014600000000     C*
014700000000     C* STAMPA IL TRAINO
014800960115IF  2C     $ERR          IFEQ      '0'                                          *NO ERRORI
014900000000     C                   EXSR      STATLT
015000000000     C*
015100960115     C* STAMPA GLI ALTRI DATI
015200000000     C                   EXSR      ELATLR                                       *ELABORA LE FERMATE
015300000000     C                   EXSR      ELATLM                                       *ELABORA I METRICUBI
015400000000     C                   EXSR      ELATLL                                       *ELABORA I LISTINI
015500000000     C                   EXSR      ELATLN                                       *ELABORA LE NOTE
015600960115E   2C                   ENDIF
015700000000     C*
015800960115     C* LETTURA SUCCESSIVA TRAINI CARIATI IN MEMORIA
015900960115     C                   ADD       1             T
016000000000E   1C                   ENDDO
016100961227     C*
016200961227     C* STAMPA RIGA DEI TOTALI LISTINI
016300961227IF  1C     TOTILI        IFGT      *ZEROS
016400961227     C     TOTIES        ORGT      *ZEROS
016500961227     C                   EXSR      STATOT
016600961227E   1C                   ENDIF
016700000000     C*
016800000000     C* STAMPA RIGA DI FINE PROSPETTO
016900000000     C                   WRITE     TL19FI
017000000000     C*
017100000000     C                   SETON                                        LR
017200000000     C*------------------------------------------------------------------------*
017300000000     C* STATLT - STAMPA RIGA TRAINO
017400000000     C*------------------------------------------------------------------------*
017500000000     C     STATLT        BEGSR
017600000000     C*
017700960115     C                   CLEAR                   TL19DE
017800960115     C*
017900960115     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI SOLO LA RIGA DI SEPARAZIONE
018000000000IF  1C     *IN01         IFEQ      *ON
018100000000     C                   MOVEL     'T'           WINT
018200000000     C                   EXSR      STATES
018300000000E   1C                   ENDIF
018400000000     C* CAMPI
018500000000     C                   Z-ADD     TLTTRN        PDETRN                         *TRAINO
018600960115     C                   SETOFF                                           28
018700960115     C                   Z-ADD     1             J
018800960115     C                   MOVEL     *BLANKS       PDEFPD                         *FILIALE PARTENZA
018900960115     C     TLTTFP        LOOKUP    ORG(J)                                 28
019000960115IF  1C     *IN28         IFEQ      *ON
019100960115     C                   MOVEL     DORG(J)       PDEFPD
019200960115E   1C                   ENDIF
019300960115     C                   SETOFF                                           28
019400960115     C                   Z-ADD     1             J
019500960115     C                   MOVEL     *BLANKS       PDEFAD                         *FILIALE ARRIVO
019600960115     C     TLTTFA        LOOKUP    ORG(J)                                 28
019700960115IF  1C     *IN28         IFEQ      *ON
019800960115     C                   MOVEL     DORG(J)       PDEFAD
019900960115E   1C                   ENDIF
020000960115     C                   SETOFF                                           28
020100960115     C                   Z-ADD     1             J
020200960115     C                   MOVEL     *BLANKS       PDETRD                         *TIPO TRAINO
020300960115     C     TLTTTR        LOOKUP    TTR(J)                                 28
020400960115IF  1C     *IN28         IFEQ      *ON
020500960115     C                   MOVEL     DTTR(J)       PDETRD
020600960115E   1C                   ENDIF
020700960115IF  1C     TLTBIS        IFEQ      'B'
020800960424     C                   MOVEL     'BIS'         PDEISD                         *NORMALE/BIS
020900960115X   1C                   ELSE
021000960424     C                   MOVEL     *BLANKS       PDEISD                         *NORMALE/BIS
021100960115E   1C                   ENDIF
021200960115     C                   MOVEA     TLTGSE        FGSE
021300960115     C                   MOVEL     '...'         PDELUN
021400960115IF  1C     FGSE(1)       IFNE      *BLANKS
021500960115     C                   MOVEL     DPAR(1)       PDELUN                         *LUNEDI'
021600960115E   1C                   ENDIF
021700960115     C                   MOVEL     '...'         PDEMAR
021800960115IF  1C     FGSE(2)       IFNE      *BLANKS
021900960115     C                   MOVEL     DPAR(2)       PDEMAR                         *MARTEDI'
022000960115E   1C                   ENDIF
022100960115     C                   MOVEL     '...'         PDEMER
022200960115IF  1C     FGSE(3)       IFNE      *BLANKS
022300960115     C                   MOVEL     DPAR(3)       PDEMER                         *MERCOLEDI'
022400960115E   1C                   ENDIF
022500960115     C                   MOVEL     '...'         PDEGIO
022600960115IF  1C     FGSE(4)       IFNE      *BLANKS
022700960115     C                   MOVEL     DPAR(4)       PDEGIO                         *GIOVEDI'
022800960115E   1C                   ENDIF
022900960115     C                   MOVEL     '...'         PDEVEN
023000960115IF  1C     FGSE(5)       IFNE      *BLANKS
023100960115     C                   MOVEL     DPAR(5)       PDEVEN                         *VENERDI'
023200960115E   1C                   ENDIF
023300960115     C                   MOVEL     '...'         PDESAB
023400960115IF  1C     FGSE(6)       IFNE      *BLANKS
023500960115     C                   MOVEL     DPAR(6)       PDESAB                         *SABATO
023600960115E   1C                   ENDIF
023700960115     C                   MOVEL     '...'         PDEDOM
023800960115IF  1C     FGSE(7)       IFNE      *BLANKS
023900960115     C                   MOVEL     DPAR(7)       PDEDOM                         *DOMENICA
024000960115E   1C                   ENDIF
024100960115     C                   MOVEL     *BLANKS       PDEMZD                         *TIPO AUTOMEZZO
024200960115     C                   SETOFF                                           28
024300960115     C                   Z-ADD     1             J
024400960115     C     TLTTMZ        LOOKUP    TMZ(J)                                 28
024500960115IF  1C     *IN28         IFEQ      *ON
024600960115     C                   MOVEL     DTMZ(J)       PDEMZD
024700960115E   1C                   ENDIF
024800960115     C                   SETOFF                                           28
024900960115     C                   Z-ADD     1             J
025000960115     C                   MOVEL     *BLANKS       PDEESD                         *FILIALE IMPORTO
025100960115     C     TLTFES        LOOKUP    ORG(J)                                 28
025200960115IF  1C     *IN28         IFEQ      *ON
025300960115     C                   MOVEL     DORG(J)       PDEESD
025400960115E   1C                   ENDIF
025500960115     C                   Z-ADD     TLTDDE        G08INV                         *DATA DECORRENZA
025600960115     C                   Z-ADD     *ZEROS        G08DAT
025700960115     C                   MOVEL     '3'           G08ERR
025800960115     C                   CALL      'XSRDA8'
025900960115     C                   PARM                    WLBDA8
026000960115     C                   Z-ADD     G08DAT        PDEDDE
026100960115     C                   Z-ADD     TLTDSC        G08INV                         *DATA SCADENZA
026200960115     C                   Z-ADD     *ZEROS        G08DAT
026300960115     C                   MOVEL     '3'           G08ERR
026400960115     C                   CALL      'XSRDA8'
026500960115     C                   PARM                    WLBDA8
026600960115     C                   Z-ADD     G08DAT        PDEDSC
026700000000     C* STAMPA
026800970423     C                   WRITE     TL19RI                                       *RIGA SEPARAZIONE
026900970423     C                   WRITE     TL19DE                                       *RIGA TRAINO
027000000000     C*
027100000000     C                   ENDSR
027200000000     C*------------------------------------------------------------------------*
027300000000     C* ELATLR - ELABORA LE FERMATE
027400000000     C*------------------------------------------------------------------------*
027500000000     C     ELATLR        BEGSR
027600000000     C*
027700000000     C* REIMPOSTA INDICATORE DI FINE FILE E FLAG PRIMA RIGA FERMATA
027800000000     C                   MOVEL     '0'           $FIN2
027900000000     C                   Z-ADD     *ZEROS        WTLR
028000000000     C*
028100000000     C* POSIZIONAMENTO E PRIMA LETTURA FERMATE DEL TRAINO
028200000000     C                   EXSR      SETFI2
028300000000     C*
028400000000     C* FINO A FINE FERMATE
028500000000DO  1C     $FIN2         DOWEQ     '0'
028600000000     C*
028700000000     C* STAMPA LA FERMATA
028800000000     C                   EXSR      STATLR
028900000000     C*
029000000000     C* LETTURA SUCESSIVA FERMATE
029100000000     C                   EXSR      LETFI2
029200000000E   1C                   ENDDO
029300000000     C*
029400000000     C                   ENDSR
029500000000     C*------------------------------------------------------------------------*
029600000000     C* SETFI2 - POSIZIONAMENTO E PRIMA LETTURA FERMATE
029700000000     C*------------------------------------------------------------------------*
029800000000     C     SETFI2        BEGSR
029900000000     C*
030000000000     C* POSIZIONAMENTO
030100000000     C                   Z-ADD     TLTTRN        KLRTRN                         *TRAINO
030200000000     C                   Z-ADD     TLTDDE        KLRDDE                         *DATA DECORRENZA
030300000000     C     KEYTLR        SETLL     TNTLR01L                           98
030400000000     C* LETTURA
030500000000IF  1C     *IN98         IFEQ      *OFF
030600000000     C                   EXSR      LETFI2
030700000000X   1C                   ELSE
030800000000     C                   MOVEL     '1'           $FIN2
030900000000E   1C                   ENDIF
031000000000     C*
031100000000     C                   ENDSR
031200000000     C*------------------------------------------------------------------------*
031300000000     C* LETFI2 - LETTURA SUCESSIVA FERMATE
031400000000     C*------------------------------------------------------------------------*
031500000000     C     LETFI2        BEGSR
031600000000     C*
031700000000     C                   MOVEL     'N'           WRECOK
031800000000     C*
031900000000     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
032000000000DO  1C     $FIN2         DOWEQ     '0'
032100000000     C     WRECOK        ANDEQ     'N'
032200000000     C     KEYTLR        READE     TNTLR01L                               98
032300000000     C*
032400000000IF  2C     *IN98         IFEQ      *ON
032500000000     C                   MOVEL     '1'           $FIN2
032600000000X   2C                   ELSE
032700000000     C                   EXSR      CHKRE2                                       *CONTROLLA RECORD
032800000000E   2C                   ENDIF
032900000000E   1C                   ENDDO
033000000000     C*
033100000000     C                   ENDSR
033200000000     C*------------------------------------------------------------------------*
033300000000     C* CHKRE2 - CONTROLLA VALIDITA' FERMATE
033400000000     C*------------------------------------------------------------------------*
033500000000     C     CHKRE2        BEGSR
033600960102     C*
033700000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
033800960102     C*
033900960102     C* FERMATA ANNULLATA, ESCLUDE
034000960102IF  1C     TLRATB        IFNE      *BLANKS
034100960102     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
034200960102E   1C                   ENDIF
034300960102     C*
034400000000     C                   ENDSR
034500000000     C*------------------------------------------------------------------------*
034600000000     C* STATLR - STAMPA RIGA FERMATA
034700000000     C*------------------------------------------------------------------------*
034800000000     C     STATLR        BEGSR
034900960115     C*
035000960115     C                   CLEAR                   TL19I2
035100960115     C                   CLEAR                   TL19D2
035200000000     C*--------------------
035300000000     C* FERMATA
035400000000     C*--------------------
035500000000     C* CAMPI
035600000000     C                   MOVEL     TLRSCA        PD2SCA                         *SCARICO
035700000000     C                   MOVEL     TLRCAR        PD2CAR                         *CARICO
035800000000     C                   Z-ADD     TLRKMT        PD2KMT                         *CHILOMETRI
035900000000     C                   Z-ADD     TLRTEM        PD2TEM                         *TEMPO
036000000000     C                   MOVEL     TLRGFV        PD2GFV                         *GIORNO FV
036100000000     C* DECODIFICHE
036200000000     C                   SETOFF                                           28
036300000000     C                   Z-ADD     1             J
036400000000     C                   MOVEL     *BLANKS       PD2ILD                         *FERMATA
036500000000     C     TLRFIL        LOOKUP    ORG(J)                                 28
036600000000IF  1C     *IN28         IFEQ      *ON
036700000000     C                   MOVEL     DORG(J)       PD2ILD
036800000000E   1C                   ENDIF
036900960115IF  1C     TLRTFM        IFEQ      'R'                                          *TIPO FERMATA RITORN
037000960115     C                   MOVEL     'R'           PD2TFM
037100000000E   1C                   ENDIF
037200000000     C*--------------------
037300000000     C* ORARI
037400000000     C*--------------------
037500960115     C                   CLEAR                   FGGA
037600960115     C                   CLEAR                   FHHA
037700960115     C                   CLEAR                   FGGP
037800960115     C                   CLEAR                   FHHP
037900000000     C*
038000000000     C* MEMORIZZA GLI ORARI DI TUTTI I GIORNI DELLA FERMATA
038100000000     C                   Z-ADD     TLRTRN        KLOTRN                         *TRAINO
038200000000     C                   Z-ADD     TLRDDE        KLODDE                         *DECORRENZA
038300000000     C                   Z-ADD     TLRNFM        KLONFM                         *NUMERO FERMATA
038400000000DO  1C     1             DO        8             J
038500960115IF  2C     FGSE(J)       IFNE      *BLANKS
038600000000     C     J             OREQ      8
038700000000IF  3C     J             IFEQ      8
038800000000     C                   MOVEL     *BLANKS       KLOGSE                         *GIORNO
038900000000X   3C                   ELSE
039000960115     C                   MOVEL     FGSE(J)       KLOGSE
039100000000E   3C                   ENDIF
039200000000     C     KEYTLO        CHAIN     TNTLO01L                           98
039300000000IF  3C     *IN98         IFEQ      *OFF
039400000000     C     TLOATB        ANDEQ     *BLANKS
039500960115     C                   MOVEL     TLOGGA        FGGA(J)
039600960115     C                   Z-ADD     TLOHHA        FHHA(J)
039700960115     C                   MOVEL     TLOGGP        FGGP(J)
039800960115     C                   Z-ADD     TLOHHP        FHHP(J)
039900000000E   3C                   ENDIF
040000000000E   2C                   ENDIF
040100000000E   1C                   ENDDO
040200000000     C* CAMPI
040300960115IF  1C     FHHA(1)       IFGT      *ZEROS                                       *ORARI DI LUNEDI'
040400960115     C     FHHP(1)       ORGT      *ZEROS
040500960115     C                   MOVEL     '¦'           PD2L11
040600960115     C                   MOVEL     DIN2(1)       I2DLUN
040700960115     C                   MOVEL     FGGA(1)       PD2GA1
040800960115     C                   Z-ADD     FHHA(1)       PD2HA1
040900960115     C                   MOVEL     '-'           PD2TR1
041000960115     C                   MOVEL     FGGP(1)       PD2GP1
041100960115     C                   Z-ADD     FHHP(1)       PD2HP1
041200960115     C                   MOVEL     '¦'           PD2L21
041300000000E   1C                   ENDIF
041400960115IF  1C     FHHA(2)       IFGT      *ZEROS                                       *ORARI DI MARTEDI'
041500960115     C     FHHP(2)       ORGT      *ZEROS
041600960115     C                   MOVEL     '¦'           PD2L12
041700960115     C                   MOVEL     DIN2(2)       I2DMAR
041800960115     C                   MOVEL     FGGA(2)       PD2GA2
041900960115     C                   Z-ADD     FHHA(2)       PD2HA2
042000960115     C                   MOVEL     '-'           PD2TR2
042100960115     C                   MOVEL     FGGP(2)       PD2GP2
042200960115     C                   Z-ADD     FHHP(2)       PD2HP2
042300960115     C                   MOVEL     '¦'           PD2L22
042400000000E   1C                   ENDIF
042500960115IF  1C     FHHA(3)       IFGT      *ZEROS                                       *ORARI DI MERCOLEDI'
042600960115     C     FHHP(3)       ORGT      *ZEROS
042700960115     C                   MOVEL     '¦'           PD2L13
042800960115     C                   MOVEL     DIN2(3)       I2DMER
042900960115     C                   MOVEL     FGGA(3)       PD2GA3
043000960115     C                   Z-ADD     FHHA(3)       PD2HA3
043100960115     C                   MOVEL     '-'           PD2TR3
043200960115     C                   MOVEL     FGGP(3)       PD2GP3
043300960115     C                   Z-ADD     FHHP(3)       PD2HP3
043400960115     C                   MOVEL     '¦'           PD2L23
043500000000E   1C                   ENDIF
043600960115IF  1C     FHHA(4)       IFGT      *ZEROS                                       *ORARI DI GIOVEDI'
043700960115     C     FHHP(4)       ORGT      *ZEROS
043800960115     C                   MOVEL     '¦'           PD2L14
043900960115     C                   MOVEL     DIN2(4)       I2DGIO
044000960115     C                   MOVEL     FGGA(4)       PD2GA4
044100960115     C                   Z-ADD     FHHA(4)       PD2HA4
044200960115     C                   MOVEL     '-'           PD2TR4
044300960115     C                   MOVEL     FGGP(4)       PD2GP4
044400960115     C                   Z-ADD     FHHP(4)       PD2HP4
044500960115     C                   MOVEL     '¦'           PD2L24
044600000000E   1C                   ENDIF
044700960115IF  1C     FHHA(5)       IFGT      *ZEROS                                       *ORARI DI VENERDI'
044800960115     C     FHHP(5)       ORGT      *ZEROS
044900960115     C                   MOVEL     '¦'           PD2L15
045000960115     C                   MOVEL     DIN2(5)       I2DVEN
045100960115     C                   MOVEL     FGGA(5)       PD2GA5
045200960115     C                   Z-ADD     FHHA(5)       PD2HA5
045300960115     C                   MOVEL     '-'           PD2TR5
045400960115     C                   MOVEL     FGGP(5)       PD2GP5
045500960115     C                   Z-ADD     FHHP(5)       PD2HP5
045600960115     C                   MOVEL     '¦'           PD2L25
045700000000E   1C                   ENDIF
045800960115IF  1C     FHHA(6)       IFGT      *ZEROS                                       *ORARI DI SABATO
045900960115     C     FHHP(6)       ORGT      *ZEROS
046000960115     C                   MOVEL     '¦'           PD2L16
046100960115     C                   MOVEL     DIN2(6)       I2DSAB
046200960115     C                   MOVEL     FGGA(6)       PD2GA6
046300960115     C                   Z-ADD     FHHA(6)       PD2HA6
046400960115     C                   MOVEL     '-'           PD2TR6
046500960115     C                   MOVEL     FGGP(6)       PD2GP6
046600960115     C                   Z-ADD     FHHP(6)       PD2HP6
046700960115     C                   MOVEL     '¦'           PD2L26
046800000000E   1C                   ENDIF
046900960115IF  1C     FHHA(7)       IFGT      *ZEROS                                       *ORARI DI DOMENICA
047000960115     C     FHHP(7)       ORGT      *ZEROS
047100960115     C                   MOVEL     '¦'           PD2L17
047200960115     C                   MOVEL     DIN2(7)       I2DDOM
047300960115     C                   MOVEL     FGGA(7)       PD2GA7
047400960115     C                   Z-ADD     FHHA(7)       PD2HA7
047500960115     C                   MOVEL     '-'           PD2TR7
047600960115     C                   MOVEL     FGGP(7)       PD2GP7
047700960115     C                   Z-ADD     FHHP(7)       PD2HP7
047800960115     C                   MOVEL     '¦'           PD2L27
047900000000E   1C                   ENDIF
048000960115IF  1C     FHHA(8)       IFGT      *ZEROS                                       *ORARI DI TUTTI I GG
048100960115     C     FHHP(8)       ORGT      *ZEROS
048200960115     C                   MOVEL     '¦'           PD2L18
048300960115     C                   MOVEL     DIN2(8)       I2DTUT
048400960115     C                   MOVEL     FGGA(8)       PD2GA8
048500960115     C                   Z-ADD     FHHA(8)       PD2HA8
048600960115     C                   MOVEL     '-'           PD2TR8
048700960115     C                   MOVEL     FGGP(8)       PD2GP8
048800960115     C                   Z-ADD     FHHP(8)       PD2HP8
048900960115     C                   MOVEL     '¦'           PD2L28
049000000000E   1C                   ENDIF
049100960115     C*
049200960115     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI L'NTESTAZIONE FERMATA
049300960115IF  1C     *IN01         IFEQ      *ON
049400960115     C                   MOVEL     'F'           WINT
049500960115     C                   EXSR      STATES
049600960115X   1C                   ELSE
049700960115IF  2C     WTLR          IFEQ      *ZEROS
049800960115     C                   WRITE     TL19I2
049900960115     C                   Z-ADD     1             WTLR
050000960115E   2C                   ENDIF
050100960115E   1C                   ENDIF
050200960115     C* STAMPA
050300000000     C                   WRITE     TL19D2                                       *RIGA FERMATA E ORAR
050400000000     C*
050500000000     C                   ENDSR
050600000000     C*------------------------------------------------------------------------*
050700000000     C* ELATLM - ELABORA I METRICUBI
050800000000     C*------------------------------------------------------------------------*
050900000000     C     ELATLM        BEGSR
051000000000     C*
051100000000     C* REIMPOSTA INDICATORE DI FINE FILE E FLAG STAMPA PRIMO METROCUBO
051200000000     C                   MOVEL     '0'           $FIN3
051300000000     C                   Z-ADD     *ZEROS        WTLM
051400000000     C*
051500000000     C* POSIZIONAMENTO E PRIMA LETTURA METRICUBI DEL TRAINO
051600000000     C                   EXSR      SETFI3
051700000000     C*
051800000000     C* FINO A FINE METRICUBI
051900000000DO  1C     $FIN3         DOWEQ     '0'
052000000000     C*
052100000000     C* STAMPA I METRICUBI
052200000000     C                   EXSR      STATLM
052300000000     C*
052400000000     C* LETTURA SUCESSIVA METRICUBI
052500000000     C                   EXSR      LETFI3
052600000000E   1C                   ENDDO
052700000000     C*
052800000000     C                   ENDSR
052900000000     C*------------------------------------------------------------------------*
053000000000     C* SETFI3 - POSIZIONAMENTO E PRIMA LETTURA METRICUBI
053100000000     C*------------------------------------------------------------------------*
053200000000     C     SETFI3        BEGSR
053300000000     C*
053400000000     C* POSIZIONAMENTO
053500000000     C                   Z-ADD     TLTTRN        KLMTRN                         *TRAINO
053600000000     C                   Z-ADD     TLTDDE        KLMDDE                         *DATA DECORRENZA
053700000000     C     KEYTLM        SETLL     TNTLM01L                           98
053800000000     C* LETTURA
053900000000IF  1C     *IN98         IFEQ      *OFF
054000000000     C                   EXSR      LETFI3
054100000000X   1C                   ELSE
054200000000     C                   MOVEL     '1'           $FIN3
054300000000E   1C                   ENDIF
054400000000     C*
054500000000     C                   ENDSR
054600000000     C*------------------------------------------------------------------------*
054700000000     C* LETFI3 - LETTURA SUCESSIVA METRICUBI
054800000000     C*------------------------------------------------------------------------*
054900000000     C     LETFI3        BEGSR
055000000000     C*
055100000000     C                   MOVEL     'N'           WRECOK
055200000000     C*
055300000000     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
055400000000DO  1C     $FIN3         DOWEQ     '0'
055500000000     C     WRECOK        ANDEQ     'N'
055600000000     C     KEYTLM        READE     TNTLM01L                               98
055700000000     C*
055800000000IF  2C     *IN98         IFEQ      *ON
055900000000     C                   MOVEL     '1'           $FIN3
056000000000X   2C                   ELSE
056100000000     C                   EXSR      CHKRE3                                       *CONTROLLA RECORD
056200000000E   2C                   ENDIF
056300000000E   1C                   ENDDO
056400000000     C*
056500000000     C                   ENDSR
056600000000     C*------------------------------------------------------------------------*
056700000000     C* CHKRE3 - CONTROLLA VALIDITA' METRICUBI
056800000000     C*------------------------------------------------------------------------*
056900000000     C     CHKRE3        BEGSR
057000000000     C*
057100000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
057200960102     C*
057300960102     C* METRICUBI ANNULLATI, ESCLUDE
057400960102IF  1C     TLMATB        IFNE      *BLANKS
057500960102     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
057600960102E   1C                   ENDIF
057700000000     C*
057800000000     C                   ENDSR
057900000000     C*------------------------------------------------------------------------*
058000000000     C* STATLM - STAMPA RIGA METRICUBI
058100000000     C*------------------------------------------------------------------------*
058200000000     C     STATLM        BEGSR
058300960115     C*
058400960115     C                   CLEAR                   TL19I3
058500960115     C                   CLEAR                   TL19D3
058600000000     C*
058700000000     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI L'INTESTAZIONE METRICUBI
058800000000IF  1C     *IN01         IFEQ      *ON
058900000000     C                   MOVEL     'M'           WINT
059000000000     C                   EXSR      STATES
059100000000X   1C                   ELSE
059200000000IF  2C     WTLM          IFEQ      *ZEROS
059300000000     C                   WRITE     TL19I3
059400000000     C                   Z-ADD     1             WTLM
059500000000E   2C                   ENDIF
059600000000E   1C                   ENDIF
059700000000     C* CAMPI
059800000000     C                   Z-ADD     TLMTFP        PD3TFP                         *FILIALE PARTENZA
059900000000     C                   Z-ADD     TLMTFA        PD3TFA                         *FILIALE SCARICO
060000000000     C                   Z-ADD     TLMTFF        PD3TFF                         *FILIALE ARRIVO
060100960424     C                   Z-ADD     TLMMCA        PD3MCA                         *MCUBI NORMALI
060200960424     C                   Z-ADD     TLMMCB        PD3MCB                         *MCUBI BIS
060300960105     C                   Z-ADD     TLMMCT        PD3MCT                         *MCUBI TRAINI SEDE
060400050928     c* incremento campi di totale metri cubi da stampare a fine elab.
060500050928     c                   add       tlmmca        tocmca
060600050928     c                   add       tlmmcb        tocmcb
060700050928     c                   add       tlmmct        tocmct
060800000000     C* DECODIFICHE
060900000000     C                   SETOFF                                           28
061000000000     C                   Z-ADD     1             J
061100000000     C                   MOVEL     *BLANKS       PD3FPD                         *FILIALE PARTENZA
061200000000     C     TLMTFP        LOOKUP    ORG(J)                                 28
061300000000IF  1C     *IN28         IFEQ      *ON
061400000000     C                   MOVEL     DORG(J)       PD3FPD
061500000000E   1C                   ENDIF
061600000000     C                   SETOFF                                           28
061700000000     C                   Z-ADD     1             J
061800000000     C                   MOVEL     *BLANKS       PD3FAD                         *FILIALE SCARICO
061900000000     C     TLMTFA        LOOKUP    ORG(J)                                 28
062000000000IF  1C     *IN28         IFEQ      *ON
062100000000     C                   MOVEL     DORG(J)       PD3FAD
062200000000E   1C                   ENDIF
062300000000     C                   SETOFF                                           28
062400000000     C                   Z-ADD     1             J
062500000000     C                   MOVEL     *BLANKS       PD3FFD                         *FILIALE ARRIVO
062600000000     C     TLMTFF        LOOKUP    ORG(J)                                 28
062700000000IF  1C     *IN28         IFEQ      *ON
062800000000     C                   MOVEL     DORG(J)       PD3FFD
062900000000E   1C                   ENDIF
063000000000     C* STAMPA
063100000000     C                   WRITE     TL19D3                                       *RIGA METRICUBI
063200000000     C*
063300000000     C                   ENDSR
063400000000     C*------------------------------------------------------------------------*
063500000000     C* ELATLL - ELABORA I LISTINI
063600000000     C*------------------------------------------------------------------------*
063700000000     C     ELATLL        BEGSR
063800000000     C*
063900000000     C* REIMPOSTA INDICATORE DI FINE FILE E FLAG STAMPA PRIMO LISTINO
064000000000     C                   MOVEL     '0'           $FIN4
064100000000     C                   Z-ADD     *ZEROS        WTLL
064200000000     C*
064300000000     C* POSIZIONAMENTO E PRIMA LETTURA LISTINI DEL TRAINO
064400000000     C                   EXSR      SETFI4
064500000000     C*
064600000000     C* FINO A FINE LISTINI
064700000000DO  1C     $FIN4         DOWEQ     '0'
064800000000     C*
064900000000     C* STAMPA I LISTINI
065000000000     C                   EXSR      STATLL
065100000000     C*
065200000000     C* LETTURA SUCESSIVA LISTINI
065300000000     C                   EXSR      LETFI4
065400000000E   1C                   ENDDO
065500000000     C*
065600000000     C                   ENDSR
065700000000     C*------------------------------------------------------------------------*
065800000000     C* SETFI4 - POSIZIONAMENTO E PRIMA LETTURA LISTINI
065900000000     C*------------------------------------------------------------------------*
066000000000     C     SETFI4        BEGSR
066100000000     C*
066200000000     C* POSIZIONAMENTO
066300000000     C                   Z-ADD     TLTTRN        KLLTRN                         *TRAINO
066400000000     C                   Z-ADD     TLTDDE        KLLDDE                         *DATA DECORRENZA
066500010725     C     KEYTLL        SETLL     TITLL01L                           98
066600000000     C* LETTURA
066700000000IF  1C     *IN98         IFEQ      *OFF
066800000000     C                   EXSR      LETFI4
066900000000X   1C                   ELSE
067000000000     C                   MOVEL     '1'           $FIN4
067100000000E   1C                   ENDIF
067200000000     C*
067300000000     C                   ENDSR
067400000000     C*------------------------------------------------------------------------*
067500000000     C* LETFI4 - LETTURA SUCESSIVA LISTINI
067600000000     C*------------------------------------------------------------------------*
067700000000     C     LETFI4        BEGSR
067800000000     C*
067900000000     C                   MOVEL     'N'           WRECOK
068000000000     C*
068100000000     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
068200000000DO  1C     $FIN4         DOWEQ     '0'
068300000000     C     WRECOK        ANDEQ     'N'
068400010725     C     KEYTLL        READE     TITLL01L                               98
068500000000     C*
068600000000IF  2C     *IN98         IFEQ      *ON
068700000000     C                   MOVEL     '1'           $FIN4
068800000000X   2C                   ELSE
068900000000     C                   EXSR      CHKRE4                                       *CONTROLLA RECORD
069000000000E   2C                   ENDIF
069100000000E   1C                   ENDDO
069200000000     C*
069300000000     C                   ENDSR
069400000000     C*------------------------------------------------------------------------*
069500000000     C* CHKRE4 - CONTROLLA VALIDITA' LISTINI
069600000000     C*------------------------------------------------------------------------*
069700000000     C     CHKRE4        BEGSR
069800000000     C*
069900000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
070000960102     C*
070100960102     C* LISTINI ANNULLATI, ESCLUDE
070200960102IF  1C     TLLATB        IFNE      *BLANKS
070300960102     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
070400960102E   1C                   ENDIF
070500000000     C*
070600000000     C                   ENDSR
070700000000     C*------------------------------------------------------------------------*
070800000000     C* STATLL - STAMPA RIGA LISTINI
070900000000     C*------------------------------------------------------------------------*
071000000000     C     STATLL        BEGSR
071100960115     C*
071200960115     C                   CLEAR                   TL19I4
071300960115     C                   CLEAR                   TL19D4
071400000000     C*
071500000000     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI L'INTESTAZIONE LISTINI
071600000000IF  1C     *IN01         IFEQ      *ON
071700000000     C                   MOVEL     'L'           WINT
071800000000     C                   EXSR      STATES
071900000000X   1C                   ELSE
072000000000IF  2C     WTLL          IFEQ      *ZEROS
072100000000     C                   WRITE     TL19I4
072200000000     C                   Z-ADD     1             WTLL
072300000000E   2C                   ENDIF
072400000000E   1C                   ENDIF
072500000000     C* CAMPI
072600000000     C                   MOVEL     TLLTMZ        PD4TMZ                         *TIPO AUTOMEZZO
072700000000     C                   Z-ADD     TLLPDR        PD4PDR                         *TRAZIONISTA
072800010725     C                   MOVEL     TLLDIV        PD4DIV                         *DIVISA
072900000000     C                   Z-ADD     TLLILI        PD4ILI                         *IMPORTO LISTINO
073000000000     C                   Z-ADD     TLLIES        PD4IES                         *IMPORTO DA ESPORRE
073100000000     C                   MOVEL     TLLCAU        PD4CAU                         *CAUSALE CONTAB.NE
073200000000     C* DECODIFICHE
073300000000     C                   SETOFF                                           28
073400000000     C                   Z-ADD     1             J
073500000000     C                   MOVEL     *BLANKS       PD4MZD                         *TIPO AUTOMEZZO
073600000000     C     TLLTMZ        LOOKUP    TMZ(J)                                 28
073700000000IF  1C     *IN28         IFEQ      *ON
073800000000     C                   MOVEL     DTMZ(J)       PD4MZD
073900000000X   1C                   ELSE
074000000000IF  2C     TLLTMZ        IFEQ      '99'
074100000000     C                   MOVEL     DES999        PD4MZD
074200000000E   2C                   ENDIF
074300000000E   1C                   ENDIF
074400000000     C                   MOVEL     *BLANKS       PD4DRD                         *TRAZIONISTA
074500000821     C                   EVAL      KLZTIP = 'T '
074600000000     C                   Z-ADD     TLLPDR        KLZPDR
074700000000     C     KEYTLZ        CHAIN     TNTLZ01L                           28
074800000000IF  1C     *IN28         IFEQ      *OFF
074900000000     C                   MOVEL     TLZRSC        PD4DRD
075000000000X   1C                   ELSE
075100000000IF  2C     TLLPDR        IFEQ      9999999
075200000000     C                   MOVEL     DES999        PD4DRD
075300000000E   2C                   ENDIF
075400000000E   1C                   ENDIF
075500000000     C                   SETOFF                                           28
075600000000     C                   Z-ADD     1             J
075700000000     C                   MOVEL     *BLANKS       PD4AUD                         *CAUSALE
075800000000     C     TLLCAU        LOOKUP    CAU(J)                                 28
075900000000IF  1C     *IN28         IFEQ      *ON
076000000000     C                   MOVEL     DCAU(J)       PD4AUD
076100000000E   1C                   ENDIF
076200000000     C* STAMPA
076300000000     C                   WRITE     TL19D4                                       *RIGA LISTINI
076400961227     C*
076500010928      * converto gli importi nella moneta di conto
076600010928     c                   if        tlldiv <> §gedcn
076700010928      * tllili
076800010928     c                   clear                   yeurcods
076900010928     c                   movel     tlldiv        yecdvi
077000010928     c                   z-add     tllili        yecimi
077100010928     c                   movel     §gedcn        yecdvo
077200010928     c                   move      §cvdec        yecdco
077300010928     c                   call      'YEURCO'
077400010928     c                   parm                    yeurcods
077500010928    5c                   if        yecesi  = ' '
077600010928     c                   z-add     yecimo        tllili
077700010928    5c                   endif
077800010928      * tllies
077900010928     c                   clear                   yeurcods
078000010928     c                   movel     tlldiv        yecdvi
078100010928     c                   z-add     tllies        yecimi
078200010928     c                   movel     §gedcn        yecdvo
078300010928     c                   move      §cvdec        yecdco
078400010928     c                   call      'YEURCO'
078500010928     c                   parm                    yeurcods
078600010928    5c                   if        yecesi  = ' '
078700010928     c                   z-add     yecimo        tllies
078800010928    5c                   endif
078900010928     c                   endif
079000961227     C* SOMMA GLI IMPORTI LISTINI -SE PIU' DI UNO PRENDE IL PRIMO-
079100961227IF  1C     $STLL         IFEQ      '0'                                          *LISTINO NON STAMPAT
079200010725     C     TOTILI        ADD       TLLILI        TOTILI           13 2
079300010725     C     TOTIES        ADD       TLLIES        TOTIES           13 2
079400961227     C                   MOVEL     '1'           $STLL
079500961227E   1C                   ENDIF
079600000000     C*
079700000000     C                   ENDSR
079800000000     C*------------------------------------------------------------------------*
079900000000     C* ELATLN - ELABORA LE NOTE
080000000000     C*------------------------------------------------------------------------*
080100000000     C     ELATLN        BEGSR
080200000000     C*
080300000000     C* REIMPOSTA INDICATORE DI FINE FILE E FLAG STAMPA PRIMA NOTA
080400000000     C                   MOVEL     '0'           $FIN5
080500000000     C                   Z-ADD     *ZEROS        WTLN
080600000000     C*
080700000000     C* POSIZIONAMENTO E PRIMA LETTURA NOTE DEL TRAINO
080800000000     C                   EXSR      SETFI5
080900000000     C*
081000000000     C* FINO A FINE NOTE
081100000000DO  1C     $FIN5         DOWEQ     '0'
081200000000     C*
081300000000     C* STAMPA LE NOTE
081400000000     C                   EXSR      STATLN
081500000000     C*
081600000000     C* LETTURA SUCESSIVA NOTE
081700000000     C                   EXSR      LETFI5
081800000000E   1C                   ENDDO
081900000000     C*
082000000000     C                   ENDSR
082100000000     C*------------------------------------------------------------------------*
082200000000     C* SETFI5 - POSIZIONAMENTO E PRIMA LETTURA NOTE
082300000000     C*------------------------------------------------------------------------*
082400000000     C     SETFI5        BEGSR
082500000000     C*
082600000000     C* POSIZIONAMENTO
082700000000     C                   Z-ADD     TLTTRN        KLNTRN                         *TRAINO
082800000000     C                   Z-ADD     TLTDDE        KLNDDE                         *DATA DECORRENZA
082900000000     C     KEYTLN        SETLL     TNTLN01L                           98
083000000000     C* LETTURA
083100000000IF  1C     *IN98         IFEQ      *OFF
083200000000     C                   EXSR      LETFI5
083300000000X   1C                   ELSE
083400000000     C                   MOVEL     '1'           $FIN5
083500000000E   1C                   ENDIF
083600000000     C*
083700000000     C                   ENDSR
083800000000     C*------------------------------------------------------------------------*
083900000000     C* LETFI5 - LETTURA SUCESSIVA NOTE
084000000000     C*------------------------------------------------------------------------*
084100000000     C     LETFI5        BEGSR
084200000000     C*
084300000000     C                   MOVEL     'N'           WRECOK
084400000000     C*
084500000000     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
084600000000DO  1C     $FIN5         DOWEQ     '0'
084700000000     C     WRECOK        ANDEQ     'N'
084800000000     C     KEYTLN        READE     TNTLN01L                               98
084900000000     C*
085000000000IF  2C     *IN98         IFEQ      *ON
085100000000     C                   MOVEL     '1'           $FIN5
085200000000X   2C                   ELSE
085300000000     C                   EXSR      CHKRE5                                       *CONTROLLA RECORD
085400000000E   2C                   ENDIF
085500000000E   1C                   ENDDO
085600000000     C*
085700000000     C                   ENDSR
085800000000     C*------------------------------------------------------------------------*
085900000000     C* CHKRE5 - CONTROLLA VALIDITA' NOTE
086000000000     C*------------------------------------------------------------------------*
086100000000     C     CHKRE5        BEGSR
086200000000     C*
086300000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
086400960102     C*
086500960102     C* NOTE ANNULLATE, ESCLUDE
086600960102IF  1C     TLNATB        IFNE      *BLANKS
086700960102     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
086800960102E   1C                   ENDIF
086900000000     C*
087000000000     C                   ENDSR
087100000000     C*------------------------------------------------------------------------*
087200000000     C* STATLN - STAMPA RIGA NOTE
087300000000     C*------------------------------------------------------------------------*
087400000000     C     STATLN        BEGSR
087500960115     C*
087600960115     C                   CLEAR                   TL19I3
087700960115     C                   CLEAR                   TL19D3
087800000000     C*
087900000000     C* SE FINE PAGINA STAMPA TESTATA, ALTRIMENTI L'INTESTAZIONE NOTE
088000000000IF  1C     *IN01         IFEQ      *ON
088100000000     C                   MOVEL     'N'           WINT
088200000000     C                   EXSR      STATES
088300000000X   1C                   ELSE
088400000000IF  2C     WTLN          IFEQ      *ZEROS
088500000000     C                   WRITE     TL19I5
088600000000     C                   Z-ADD     1             WTLN
088700000000E   2C                   ENDIF
088800000000E   1C                   ENDIF
088900000000     C* CAMPI
089000000000     C                   Z-ADD     TLNNFM        PD5NFM                         *NUMERO FERMATA
089100000000     C                   MOVEL     TLNTNO        PD5TNO                         *TESTO
089200000000     C* STAMPA
089300000000     C                   WRITE     TL19D5                                       *RIGA NOTE
089400000000     C*
089500000000     C                   ENDSR
089600000000     C*------------------------------------------------------------------------*
089700000000     C* STATES - STAMPA TESTATA
089800000000     C*          INPUT -> WINT: TIPO INTESTAZIONE
089900000000     C*------------------------------------------------------------------------*
090000000000     C     STATES        BEGSR
090100000000     C*
090200000000     C                   WRITE     TL19TE                                       *TESTATA
090300000000     C*
090400000000     C* STAMPA IL TIPO DI INTESTAZIONE RICHIESTO
090500000000IF  1C     WINT          IFEQ      'F'
090600000000     C                   WRITE     TL19I2                                       *INTESTAZIONE FERMAT
090700960116     C                   Z-ADD     1             WTLR
090800000000E   1C                   ENDIF
090900000000IF  1C     WINT          IFEQ      'M'
091000000000     C                   WRITE     TL19I3                                       *INTESTAZIONE MCUBI
091100960116     C                   Z-ADD     1             WTLM
091200000000E   1C                   ENDIF
091300000000IF  1C     WINT          IFEQ      'L'
091400000000     C                   WRITE     TL19I4                                       *INTESTAZIONE LISTIN
091500960116     C                   Z-ADD     1             WTLL
091600000000E   1C                   ENDIF
091700000000IF  1C     WINT          IFEQ      'N'
091800000000     C                   WRITE     TL19I5                                       *INTESTAZIONE NOTE
091900960116     C                   Z-ADD     1             WTLN
092000000000E   1C                   ENDIF
092100000000     C                   MOVEL     '0'           *IN01
092200000000     C*
092300000000     C                   ENDSR
092400961227     C*------------------------------------------------------------------------*
092500961227     C* STATOT - STAMPA RIGA DEI TOTALI LISTINI
092600961227     C*------------------------------------------------------------------------*
092700961227     C     STATOT        BEGSR
092800961227     C*
092900970423     C                   WRITE     TL19RI                                       *RIGA SEPARAZIONE
093000961227     C                   WRITE     TL19TE                                       *TESTATA
093100961227     C                   WRITE     TL19RI                                       *RIGA SEPARAZIONE
093200961227     C*
093300961227     C                   Z-ADD     TOTILI        TOCILI
093400961227     C                   Z-ADD     TOTIES        TOCIES
093500011001     c                   if        totili <> *zeros
093600010928     c                   movel     §gedcn        todili
093700011001     c                   endif
093800011001     c                   if        toties <> *zeros
093900010928     c                   movel     §gedcn        todies
094000011001     c                   endif
094100961227     C                   WRITE     TL19TO                                       *TOTALI
094200961227     C*
094300961227     C                   WRITE     TL19RI                                       *RIGA SEPARAZIONE
094400961227     C*
094500961227     C                   ENDSR
094600960115     C*------------------------------------------------------------------------*
094700960115     C* ELATLT - MEMORIZZA I TRAINI DA STAMPARE
094800960115     C*------------------------------------------------------------------------*
094900960115     C     ELATLT        BEGSR
095000960115     C*
095100960115     C* POSIZIONAMENTO E PRIMA LETTURA TRAINI
095200960115     C                   Z-ADD     *ZEROS        T                 5 0
095300960115     C                   EXSR      SETFIL
095400960115     C*
095500960115     C* FINO A FINE TRAINI
095600960115DO  1C     $FINE         DOWEQ     '0'
095700960115     C*
095800960115     C                   EXSR      MEMTLT
095900960115     C*
096000960115     C* LETTURA SUCESSIVA TRAINI
096100960115     C                   EXSR      LETFIL
096200960115E   1C                   ENDDO
096300960116     C*
096400960116     C* ORDINA I TRAINI PER LA STAMPA
096500960116     C                   EXSR      ORDTLT
096600960115     C*
096700960115     C                   ENDSR
096800960115     C*------------------------------------------------------------------------*
096900960115     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA TRAINI
097000960115     C*------------------------------------------------------------------------*
097100960115     C     SETFIL        BEGSR
097200960115     C*
097300960115     C* POSIZIONAMENTO
097400960115     C     *LOVAL        SETLL     TNTLT01L                           99        *FINE FILE
097500960115     C* RPIMA LETTURA
097600960115IF  1C     *IN99         IFEQ      *OFF
097700960115     C                   EXSR      LETFIL
097800960115X   1C                   ELSE
097900960115     C                   MOVEL     '1'           $FINE
098000960115E   1C                   ENDIF
098100960115     C*
098200960115     C                   ENDSR
098300960115     C*------------------------------------------------------------------------*
098400960115     C* LETFIL - LETTURA SUCESSIVA TRAINI
098500960115     C*------------------------------------------------------------------------*
098600960115     C     LETFIL        BEGSR
098700960115     C*
098800960115     C                   MOVEL     'N'           WRECOK
098900960115     C*
099000960115     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
099100960115DO  1C     $FINE         DOWEQ     '0'
099200960115     C     WRECOK        ANDEQ     'N'
099300960115     C                   READ      TNTLT01L                               99
099400960115IF  2C     *IN99         IFEQ      *ON
099500960115     C                   MOVEL     '1'           $FINE
099600960115X   2C                   ELSE
099700960115     C                   EXSR      CHKREC                                       *CONTROLLA RECROD
099800960115E   2C                   ENDIF
099900960115E   1C                   ENDDO
100000960115     C*
100100960115     C                   ENDSR
100200960115     C*------------------------------------------------------------------------*
100300960115     C* CHKREC - CONTROLLA VALIDITA' TRAINI
100400960115     C*------------------------------------------------------------------------*
100500960115     C     CHKREC        BEGSR
100600960115     C*
100700960115     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
100800960115     C*
100900960115     C* TRAINO ANNULLATO, ESCLUDE
101000960115IF  1C     TLTATB        IFNE      *BLANKS
101100960115     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
101200960115E   1C                   ENDIF
101300960115     C*
101400960115     C* TRAINO NON IN DECORRENZA ALLA DATA RICHIESTA, ESCLUDE
101500060323IF  1C**   PARDDE        IFGT      *ZEROS
101600060323IF  2C**   PARDDE        IFGE      TLTDDE
101700060323     C**   PARDDE        ANDLE     TLTDSC
101800060323X   2C**                 ELSE
101900060323     C**                 MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
102000060323E   2C**                 ENDIF
102100060323E   1C**                 ENDIF
102200060323     c* Traino scaduto prima della data dal o decorrente dopo la data al, es
102300060323     c* clude
102400060323     c     tltdsc        iflt      pardda
102500060323     c     tltdde        orgt      pardde
102600060323     c                   movel     'N'           wrecok
102700060323     c                   endif
102800961021     C*
102900961021     C* NUMERO TRAINO MINORE DEL NUMERO TRAINO DAL RICHIESTO,  ESCLUDE
103000961021IF  1C     PARTR1        IFGT      *ZEROS
103100961021     C     TLTTRN        ANDLT     PARTR1
103200961021     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
103300961021E   1C                   ENDIF
103400961021     C*
103500961021     C* NUMERO TRAINO MAGGIORE DEL NUMERO TRAINO AL RICHIESTO,  ESCLUDE
103600961021IF  1C     PARTR2        IFGT      *ZEROS
103700961021     C     TLTTRN        ANDGT     PARTR2
103800961021     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
103900961021E   1C                   ENDIF
104000961021     C*
104100960115     C* GIORNO PARTENZA <> DA QUELLO RICHIESTO, ESCLUDE
104200960115IF  1C     PARGIO        IFNE      *BLANKS
104300960115     C     PARGIO        SCAN      TLTGSE                                 28
104400960115IF  2C     *IN28         IFEQ      *OFF
104500960115     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
104600960115E   2C                   ENDIF
104700960115E   1C                   ENDIF
104800060323     c* trazionista <> da quello richiesto, esclude
104900060323     c     parpdr        ifgt      *zeros
105000060323     C                   Z-ADD     TLTTRN        KLLTRN                         *TRAINO
105100060323     C                   Z-ADD     TLTDDE        KLLDDE                         *DATA DECORRENZA
105200060323     C                   Z-ADD     parpdr        KLLpdr                         *DATA DECORRENZA
105300060323     C     KEYTLL2       SETLL     TITLL02L
105400060323     c                   if        not %equal(titll02l)
105500060323     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
105600060323     c                   endif
105700060323     c                   endif
105800060323     c* se richiesto bis e traino non bis, esclude
105900060323     c* se richiesti i non bis e traino bis, esclude
106000060323     c                   if        (parbis='S' and tltbis <> 'B' and
106100060323     c                              tlttrn < 9000000) or
106200060323     c                             (parbis='E' and (tltbis='B' or
106300060323     c                              tlttrn >=9000000))
106400060323     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
106500060323     c                   endif
106600060323     c
106700960115     C*--------------------
106800960115     C* FERMATE
106900960115     C*--------------------
107000960115     C***
107100960213     C* TRAINI DI UNA FILIALE ==> FERMATA ALLA FILIALE (SE RICHIESTA)
107200960115     C* TRAINI PARTENZA ==> SOLO FERMATE CHE CARICANO  ALLA FILIALE RICHIESTA
107300960115     C* TRAINI ARRIVO   ==> SOLO FERMATE CHE SCARICANO ALLA FILIALE RICHIESTA
107400960115     C***
107500960115     C                   Z-ADD     *ZEROS        DEPNFM
107600960115IF  1C     WRECOK        IFEQ      'S'                                          *TRAINO ANCORA VALID
107700960213IF  2C     PARORD        IFEQ      '3'                                          *ORD X ORARIO PART
107800960213     C     PARORD        OREQ      '4'                                          *ORD X ORARIO ARR
107900960213     C     PARORD        OREQ      '2'                                          *ORD X FILIALE
108000960213     C     PARFIL        ANDGT     *ZEROS                                       *SE RICH UNA FILIALE
108100960115     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
108200960115     C                   Z-ADD     TLTTRN        KLRTRN                         *TRAINO
108300960115     C                   Z-ADD     TLTDDE        KLRDDE                         *DATA DECORRENZA
108400960115     C     KEYTLR        SETLL     TNTLR01L                           98
108500960115     C     KEYTLR        READE     TNTLR01L                               98
108600960115DO  3C     *IN98         DOWEQ     *OFF
108700960115     C     WRECOK        ANDEQ     'N'
108800960115IF  4C     TLRATB        IFEQ      *BLANKS                                      *NON ANULLATA
108900960115IF  5C     PARFIL        IFEQ      TLRFIL
109000960118IF  6C     PARORD        IFEQ      '3'
109100960116     C     TLRCAR        ANDEQ     'S'
109200960118     C     PARORD        OREQ      '4'
109300960116     C     TLRSCA        ANDEQ     'S'
109400960213     C     PARORD        OREQ      '2'
109500960115     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
109600960115     C                   Z-ADD     TLRNFM        DEPNFM                         *NUMERO FERMATA
109700960115E   6C                   ENDIF
109800960115E   5C                   ENDIF
109900960115E   4C                   ENDIF
110000960115     C     KEYTLR        READE     TNTLR01L                               98
110100960115E   3C                   ENDDO
110200960115E   2C                   ENDIF
110300960115E   1C                   ENDIF
110400960115     C*
110500960115     C                   ENDSR
110600960115     C*------------------------------------------------------------------------*
110700960115     C* MEMTLT - MEMORIZZA IN SCHIERE I TRAINI VALIDI
110800960115     C*------------------------------------------------------------------------*
110900960115     C     MEMTLT        BEGSR
111000960115     C* TRAINO
111100960115     C                   ADD       1             T
111200960115     C                   Z-ADD     TLTTRN        TTRN(T)
111300960115     C                   Z-ADD     TLTDDE        TDDE(T)
111400960118     C                   Z-ADD     TLTTFP        TTFP(T)
111500960115     C* ORARI
111600960118IF  1C     PARORD        IFEQ      '3'
111700960118     C     PARORD        OREQ      '4'
111800960118     C                   Z-ADD     TLTTRN        KLOTRN                         *TRAINO VALIDO
111900960115     C                   Z-ADD     TLTDDE        KLODDE
112000960115     C                   Z-ADD     DEPNFM        KLONFM                         *FERMATA DEPOSITATA
112100960115     C                   MOVEL     PARGIO        KLOGSE                         *GIORNO SELEZIONATO
112200960115     C     KEYTLO        CHAIN     TNTLO01L                           98
112300960115IF  2C     *IN98         IFEQ      *ON
112400960115     C                   MOVEL     *BLANKS       KLOGSE                         *GIORNO "TUTTI"
112500960115     C     KEYTLO        CHAIN     TNTLO01L                           98
112600960115E   2C                   ENDIF
112700960115IF  2C     *IN98         IFEQ      *OFF
112800960115     C                   MOVEL     TLOGGA        TGHA(T)                        *ARRIVO
112900960115     C                   MOVE      TLOHHA        TGHA(T)
113000960115     C                   MOVEL     TLOGGP        TGHP(T)                        *PARTENZA
113100960115     C                   MOVE      TLOHHP        TGHP(T)
113200960115E   2C                   ENDIF
113300960115E   1C                   ENDIF
113400960115     C*
113500960115     C                   ENDSR
113600960116     C*------------------------------------------------------------------------*
113700960116     C* ORDTLT - ORDINA I TRAINI PER LA STAMPA
113800960116     C*------------------------------------------------------------------------*
113900960116     C     ORDTLT        BEGSR
114000960118     C*
114100960118DO  1C     1             DO        T             J                 4 0
114200960116     C     1             ADD       J             K
114300960118     C*
114400960118DO  2C     K             DO        T             K                 4 0
114500960116     C*
114600960118IF  3C     PARORD        IFEQ      '1'                                          *SECONDO ORDINAMENTO
114700960118     C     TTRN(J)       ANDGT     TTRN(K)                                       TRAINO
114800960118     C     PARORD        OREQ      '2'                                          *SECONDO ORDINAMENTO
114900960118     C     TTFP(J)       ANDGT     TTFP(K)                                       FILIALE
115000960118     C     PARORD        OREQ      '3'                                          *TERZO ORDINAMENTO
115100960118     C     TGHP(J)       ANDGT     TGHP(K)                                       ORARIO PARTENZA
115200960118     C     PARORD        OREQ      '4'                                          *QUARDO ORDINAMENTO
115300960118     C     TGHA(J)       ANDGT     TGHA(K)                                       ORARIO ARRIVO
115400960116     C*
115500960116     C                   MOVEL     TGHP(J)       WGHP              5            *GIORNO+ORA PARTENZA
115600960116     C                   MOVEL     TGHP(K)       TGHP(J)
115700960116     C                   MOVEL     WGHP          TGHP(K)
115800960116     C*
115900960116     C                   MOVEL     TGHA(J)       WGHA              5            *GIORNO+ORA ARRIVO
116000960116     C                   MOVEL     TGHA(K)       TGHA(J)
116100960116     C                   MOVEL     WGHA          TGHA(K)
116200960116     C*
116300960116     C                   Z-ADD     TTRN(J)       WTRN              7 0          *NUMERO TRAINO
116400960116     C                   Z-ADD     TTRN(K)       TTRN(J)
116500960116     C                   Z-ADD     WTRN          TTRN(K)
116600960116     C*
116700960116     C                   Z-ADD     TDDE(J)       WDDE              8 0          *DATA DECORRENZA
116800960116     C                   Z-ADD     TDDE(K)       TDDE(J)
116900960116     C                   Z-ADD     WDDE          TDDE(K)
117000960118     C*
117100960118     C                   Z-ADD     TTFP(J)       WTFP              3 0          *FILIALE
117200960118     C                   Z-ADD     TTFP(K)       TTFP(J)
117300960118     C                   Z-ADD     WTFP          TTFP(K)
117400960116     C*
117500960118E   3C                   ENDIF
117600960118E   2C                   ENDDO
117700960118E   1C                   ENDDO
117800960116     C*
117900960116     C                   ENDSR
118000000000     C*------------------------------------------------------------------------*
118100000000     C* NEWTLT - OPERAZIONI PER NUOVO TRAINO
118200000000     C*------------------------------------------------------------------------*
118300000000     C     NEWTLT        BEGSR
118400960115     C*
118500960115     C                   MOVEL     '0'           $ERR
118600000000     C*
118700000000     C* MEMORIZZA IL NUOVO TRAINO IN UN DEPOSITO
118800960115     C                   Z-ADD     TTRN(T)       DEPTRN                         *TRAINO
118900960115     C                   Z-ADD     TDDE(T)       DEPDDE                         *DATA DECORRENZA
119000960115     C*
119100960115     C* LEGGE IL TRAINO MEMORIZZATO
119200960115     C                   Z-ADD     TTRN(T)       KLTTRN
119300960115     C                   Z-ADD     TDDE(T)       KLTDDE
119400960115     C     KEYTLT        CHAIN     TNTLT01L                           99        *FINE FILE
119500960115IF  1C     *IN99         IFEQ      *ON
119600960115     C                   MOVEL     '1'           $ERR
119700960115E   1C                   ENDIF
119800961227     C*
119900961227     C* FLAG DI PASSAGGIO STAMPA LISTINO
120000961227     C                   MOVEL     '0'           $STLL                          *STAMPA LISTINO
120100000000     C*
120200000000     C                   ENDSR
120300000000     C*------------------------------------------------------------------------*
120400000000     C* STAPAR - STAMPA PAGINA PARAMETRI
120500000000     C*------------------------------------------------------------------------*
120600000000     C     STAPAR        BEGSR
120700960115     C* PARAMETRI
120800960118     C                   MOVEL     PARORD        PACORD
120900960118IF  1C     PARORD        IFEQ      '1'
121000960118     C                   MOVEL     DPAR(11)      PADORD
121100960115E   1C                   ENDIF
121200960118IF  1C     PARORD        IFEQ      '2'
121300960118     C                   MOVEL     DPAR(12)      PADORD
121400960118E   1C                   ENDIF
121500960118IF  1C     PARORD        IFEQ      '3'
121600960118     C                   MOVEL     DPAR(13)      PADORD
121700960118E   1C                   ENDIF
121800960118IF  1C     PARORD        IFEQ      '4'
121900960118     C                   MOVEL     DPAR(14)      PADORD
122000960118E   1C                   ENDIF
122100960118     C*
122200060323IF  1C     PARDDa        IFGT      *ZEROS
122300060323     C                   Z-ADD     PARDDa        G08INV                         *DECORRENZA
122400060323     C                   Z-ADD     *ZEROS        G08DAT
122500060323     C                   MOVEL     '3'           G08ERR
122600060323     C                   CALL      'XSRDA8'
122700060323     C                   PARM                    WLBDA8
122800060323     C                   Z-ADD     G08DAT        PACDDA
122900060323E   1C                   ENDIF
123000960115IF  1C     PARDDE        IFGT      *ZEROS
123100000000     C                   Z-ADD     PARDDE        G08INV                         *DECORRENZA
123200000000     C                   Z-ADD     *ZEROS        G08DAT
123300000000     C                   MOVEL     '3'           G08ERR
123400000000     C                   CALL      'XSRDA8'
123500000000     C                   PARM                    WLBDA8
123600000000     C                   Z-ADD     G08DAT        PACDDE
123700960115E   1C                   ENDIF
123800960115IF  1C     PARFIL        IFGT      *ZEROS
123900960115     C                   Z-ADD     PARFIL        PACFIL                         *FILIALE
124000960115     C                   Z-ADD     1             J
124100960115     C     PARFIL        LOOKUP    ORG(J)                                 28
124200960115     C                   MOVEL     DORG(J)       PADFIL
124300960115E   1C                   ENDIF
124400960115IF  1C     PARGIO        IFNE      *BLANKS
124500960115     C                   MOVEL     PARGIO        PACGIO                         *GIORNO DI PARTENZA
124600960115     C                   MOVEL     PARGIO        N1                1 0
124700960115     C                   MOVEL     DPAR(N1)      PADGIO
124800960115E   1C                   ENDIF
124900961021     C                   Z-ADD     PARTR1        PACTR1                         *NUMERO TRAINO DAL
125000961021     C                   Z-ADD     PARTR2        PACTR2                         *NUMERO TRAINO AL
125100960115     C* TESTATA
125200960118     C                   MOVEL     PADORD        TEDORD
125300060323IF  1C     PARDDA        IFGT      *ZEROS
125400060323     C                   MOVEL     'DAL'         TIDDDA
125500060323     C                   Z-ADD     PACDDA        TECDDA
125600060323E   1C                   ENDIF
125700960118IF  1C     PARDDE        IFGT      *ZEROS
125800960118     C                   MOVEL     'AL'          TIDDDE
125900960118     C                   Z-ADD     PACDDE        TECDDE
126000960118E   1C                   ENDIF
126100960116IF  1C     PARFIL        IFGT      *ZEROS
126200960116     C                   MOVEL     'DA'          TIDFIL
126300960116     C                   MOVEL     PADFIL        TEDFIL
126400960116E   1C                   ENDIF
126500060323     c* trazionista
126600060323     c                   clear                   padpdr
126700060323     c                   clear                   pacpdr
126800060323     c                   if        parpdr > *zeros
126900060323     c                   move      parpdr        pacpdr
127000060323     C                   EVAL      KLZTIP = 'T '
127100060323     C                   z-add     parpdr        KLZPDR
127200060323     C     KEYTLZ        CHAIN     TNTLZ01L                           28
127300060323IF  2C     *IN28         IFEQ      *OFF
127400060323     C                   MOVEL     TLZRSC        padpdr                         *DESCR.NE TRAZIONIST
127500060323     c                   endif
127600060323     c                   endif
127700060323     c* traini bis si/no/escludi
127800060323     c                   clear                   padbis
127900060323     c                   select
128000060323     c                   when      parbis = 'S'
128100060323     c                   eval      padbis = 'Si'
128200060323     c                   when      parbis = 'N'
128300060323     c                   eval      padbis = 'No'
128400060323     c                   when      parbis = 'E'
128500060323     c                   eval      padbis = 'Escludi'
128600060323     c                   endsl
128700960115     C*
128800000000     C                   WRITE     TL19TE                                       *TESTATA
128900000000     C                   WRITE     TL19PA                                       *PARAMETRI
129000000000     C*
129100000000     C                   ENDSR
129200000000     C*------------------------------------------------------------------------*
129300000000     C* CARTAB - CARICA LE TABELLE
129400000000     C*------------------------------------------------------------------------*
129500000000     C     CARTAB        BEGSR
129600000000     C*--------------------
129700000000     C* FILIALI
129800000000     C*--------------------
129900000000     C                   Z-ADD     *ZEROS        J                 4 0
130000000000     C     *LOVAL        SETLL     AZORG01L
130100000000     C                   READ      AZORG01L                               99
130200000000DO  1C     *IN99         DOWEQ     *OFF
130300000000IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
130400000000     C                   ADD       1             J
130500000000     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
130600000000     C                   MOVEL     ORGDES        DORG(J)                        *DESCRIZIONE
130700000000E   2C                   ENDIF
130800000000     C                   READ      AZORG01L                               99
130900000000E   1C                   ENDDO
131000000000     C*--------------------
131100000000     C* TIPI TRAINO
131200000000     C*--------------------
131300000000     C                   Z-ADD     *ZEROS        J
131400020715     c                   Z-Add     1             KtbKut
131500000000     C                   MOVEL     'TV'          KTBCOD
131600000000     C     KE2TAB        SETLL     TABEL00F
131700000000     C     KE2TAB        READE     TABEL00F                               99
131800000000DO  1C     *IN99         DOWEQ     *OFF
131900000000IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
132000000000     C                   ADD       1             J
132100000000     C                   MOVEL     TBLKEY        TTR(J)                         *CODICE
132200000000     C                   MOVEL     TBLUNI        DSTV
132300000000     C                   MOVEL     §TVDES        DTTR(J)                        *DESCRIZIONE
132400000000E   2C                   ENDIF
132500000000     C     KE2TAB        READE     TABEL00F                               99
132600000000E   1C                   ENDDO
132700000000     C*--------------------
132800000000     C* TIPI AUTOMEZZO
132900000000     C*--------------------
133000000000     C                   Z-ADD     *ZEROS        J
133100020715     c                   Z-Add     1             KtbKut
133200000000     C                   MOVEL     'TU'          KTBCOD
133300000000     C     KE2TAB        SETLL     TABEL00F
133400000000     C     KE2TAB        READE     TABEL00F                               99
133500000000DO  1C     *IN99         DOWEQ     *OFF
133600000000IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
133700000000     C                   ADD       1             J
133800000000     C                   MOVEL     TBLKEY        TMZ(J)                         *CODICE
133900000000     C                   MOVEL     TBLUNI        DSTU
134000000000     C                   MOVEL     §TUDES        DTMZ(J)                        *DESCRIZIONE
134100000000E   2C                   ENDIF
134200000000     C     KE2TAB        READE     TABEL00F                               99
134300000000E   1C                   ENDDO
134400000000     C*--------------------
134500000000     C* CAUSALI CONTABILIZZAZIONE
134600000000     C*--------------------
134700020715     c                   If        DutLpo = 'S'
134800000821     C                   OPEN      ANATB01L
134900000000     C                   Z-ADD     *ZEROS        J
135000000821     C                   MOVE      KNSIF         WCOMO4            4
135100000821     C                   MOVEL     WCOMO4        WSOC              3
135200000821     C                   movel     WSOC          KAtazi
135300000211     C                   MOVEL     'Y4Z'         KatCOD
135400000211     C     KEATAB        CHAIN     ANATB01L                           99
135500000821IF  2C     *IN99         IFEQ      *ON
135600000218     C                   movel     '888'         KAtazi
135700000211     C     KEATAB        CHAIN     ANATB01L                           99
135800000821E   2C                   ENDIF
135900000821DO  2C     *IN99         DOWEQ     *OFF
136000000211     C                   ADD       1             J
136100000211     C                   MOVEL     ATBKEY        CAU(J)
136200000211     C                   MOVEL     ATBUNI        ANGY4ZDS
136300000211     C                   MOVEL     §4ZDES        DCAU(J)
136400000211     C     KEATAB        READE     ANATB01L                               99
136500000821E   2C                   ENDDO
136600000821     C                   CLOSE     ANATB01L
136700000821E   1C                   ENDIF
136800010928      *--------------------
136900010928      * Moneta di conto
137000010928      *--------------------
137100010928     c                   clear                   dged
137200010928     c                   clear                   tibs02ds
137300010928     c                   movel     'L'           t02tla
137400010928     c                   movel     'C'           t02mod
137500010928     c                   movel     knsif         t02sif
137600010928     c                   movel     'GED'         t02cod
137700010928     c                   movel     '1'           t02ke1
137800010928     c                   call      'TIBS02R'
137900010928     c                   parm                    kpjba
138000010928     c                   parm                    tibs02ds
138100010928    5c                   movel     t02uni        dged
138200010928
138300010928     c                   clear                   dscv
138400010928     c                   movel     'CV'          ktbcod
138500010928     c                   movel(p)  §gedcn        ktbkey
138600010928     c     ktab          chain     tabel00f
138700010928     c                   if        %found(tabel00f)
138800010928     c                   movel     tbluni        dscv
138900010928     c                   endif
139000010928
139100000000     C*
139200000000     C                   ENDSR
139300000000     C*------------------------------------------------------------------------*
139400000000     C* *INZSR - ROUTINE INIZIALE
139500000000     C*------------------------------------------------------------------------*
139600000000     C     *INZSR        BEGSR
139700000000     C*--------------------
139800000000     C* RICEVIMENTO PARAMETRI
139900000000     C*--------------------
140000000000     C     *ENTRY        PLIST
140100000000     C                   PARM                    KPJBA
140200000000     C                   MOVEL     KPJBU         PARAM
140300000000     C*--------------------
140400000000     C* VARIABILI RIFERITE AL DATA BASE
140500000000     C*--------------------
140600000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
140700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
140800010928     C     *LIKE         DEFINE    TBLkey        KTBkey
140900000211     C     *LIKE         DEFINE    ATBCOD        KATCOD
141000000211     C     *LIKE         DEFINE    ATBAZI        KATAZI
141100000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
141200000000     C     *LIKE         DEFINE    TLZPDR        KLZPDR                         *TNTLZ00F
141300000821     C     *LIKE         DEFINE    TLZTIP        KLZTIP
141400000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
141500000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
141600000000     C     *LIKE         DEFINE    TLRTRN        KLRTRN                         *TNTLR00F
141700000000     C     *LIKE         DEFINE    TLRDDE        KLRDDE
141800000000     C     *LIKE         DEFINE    TLOTRN        KLOTRN                         *TNTLO00F
141900000000     C     *LIKE         DEFINE    TLODDE        KLODDE
142000000000     C     *LIKE         DEFINE    TLONFM        KLONFM
142100000000     C     *LIKE         DEFINE    TLOGSE        KLOGSE
142200000000     C     *LIKE         DEFINE    TLMTRN        KLMTRN                         *TNTLM00F
142300000000     C     *LIKE         DEFINE    TLMDDE        KLMDDE
142400010725     C     *LIKE         DEFINE    TLLTRN        KLLTRN                         *TITLL00F
142500000000     C     *LIKE         DEFINE    TLLDDE        KLLDDE
142600060323     C     *LIKE         DEFINE    TLLpdr        KLLpdr
142700000000     C     *LIKE         DEFINE    TLNTRN        KLNTRN                         *TNTLN00F
142800000000     C     *LIKE         DEFINE    TLNDDE        KLNDDE
142900000000     C     *LIKE         DEFINE    TLTTRN        DEPTRN                         *DEPOSITI
143000000000     C     *LIKE         DEFINE    TLTDDE        DEPDDE
143100960116     C     *LIKE         DEFINE    TLRNFM        DEPNFM
143200000000     C*--------------------
143300000000     C* CHIAVI DI LETTURA
143400000000     C*--------------------
143500000000     C* LETTURA AZORG01L - COMPLETA
143600000000     C     KEYORG        KLIST
143700000000     C                   KFLD                    KORFIL                         *FILIALE
143800000000     C* LETTURA TNTAB00F - PARZIALE
143900000000     C     KE2TAB        KLIST
144000000000     C                   KFLD                    KTBKUT                         *UTENTE
144100000000     C                   KFLD                    KTBCOD                         *TABELLA
144200010928
144300010928     C     KTAB          KLIST
144400010928     C                   KFLD                    ktbkut
144500010928     C                   KFLD                    Ktbcod
144600010928     C                   KFLD                    Ktbkey
144700000211     C* LETTURA ANATB01L - PARZIALE
144800000211     C     KEATAB        KLIST
144900000211     C                   KFLD                    KATCOD
145000000211     C                   KFLD                    KATAZI
145100000000     C* LETTURA TNTLZ01L - COMPLETA
145200000000     C     KEYTLZ        KLIST
145300000821     C                   KFLD                    KLZTIP                         *TIPO
145400000821     C                   KFLD                    KLZPDR                         *CODICE
145500000000     C* LETTURA TNTLT01L - COMPLETA
145600000000     C     KEYTLT        KLIST
145700000000     C                   KFLD                    KLTTRN                         *TRAINO
145800000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
145900000000     C* LETTURA TNTLR01L - PARZIALE
146000000000     C     KEYTLR        KLIST
146100000000     C                   KFLD                    KLRTRN                         *TRAINO
146200000000     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
146300000000     C* LETTURA TNTLO01L - COMPLETA
146400000000     C     KEYTLO        KLIST
146500000000     C                   KFLD                    KLOTRN                         *TRAINO
146600000000     C                   KFLD                    KLODDE                         *DATA DECORRENZA
146700000000     C                   KFLD                    KLONFM                         *NUMERO FERMATA
146800000000     C                   KFLD                    KLOGSE                         *GIORNO DI PARTENZA
146900000000     C* LETTURA TNTLM01L - PARZIALE
147000000000     C     KEYTLM        KLIST
147100000000     C                   KFLD                    KLMTRN                         *TRAINO
147200000000     C                   KFLD                    KLMDDE                         *DATA DECORRENZA
147300010725     C* LETTURA TITLL01L - PARZIALE
147400000000     C     KEYTLL        KLIST
147500000000     C                   KFLD                    KLLTRN                         *TRAINO
147600000000     C                   KFLD                    KLLDDE                         *DATA DECORRENZA
147700060323     C* LETTURA TITLL02L - PARZIALE
147800060323     C     KEYTLL2       KLIST
147900060323     C                   KFLD                    KLLpdr                         *TRAINO
148000060323     C                   KFLD                    KLLTRN                         *TRAINO
148100060323     C                   KFLD                    KLLDDE                         *DATA DECORRENZA
148200000000     C* LETTURA TNTLN01L - PARZIALE
148300000000     C     KEYTLN        KLIST
148400000000     C                   KFLD                    KLNTRN                         *TRAINO
148500000000     C                   KFLD                    KLNDDE                         *DATA DECORRENZA
148600000000     C*--------------------
148700000000     C* VARIABILI DI CONTROLLO
148800000000     C*--------------------
148900000000     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA TRAINI
149000000000     C                   MOVEL     '0'           $FIN2             1            *FINE LETTURA FERMAT
149100000000     C                   MOVEL     '0'           $FIN3             1            *FINE LETTURA MCUBI
149200000000     C                   MOVEL     '0'           $FIN4             1            *FINE LETTURA LISTIN
149300000000     C                   MOVEL     '0'           $FIN5             1            *FINE LETTURA NOTE
149400000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
149500000000     C                   MOVEL     '1'           *IN01                          *OVERFLOW
149600000000     C                   MOVEL     *BLANKS       WINT              1            *INTESTAZ. IN STAMPA
149700000000     C                   Z-ADD     *ZEROS        WTLR              1 0          *STAMPA 1^FERMATA
149800000000     C                   Z-ADD     *ZEROS        WTLM              1 0          *STAMPA 1^MCUBO
149900000000     C                   Z-ADD     *ZEROS        WTLL              1 0          *STAMPA 1^LISTINO
150000000000     C                   Z-ADD     *ZEROS        WTLN              1 0          *STAMPA 1^NOTA
150100960115     C                   MOVEL     '0'           $ERR              1            *ERRORE IN LETTURA
150200961227     C                   MOVEL     '0'           $STLL             1            *STAMPA LISTINO
150300000000     C*
150400000000     C* REPERISCE I DATI UTENTE
150500020715
150600020715     c     *dtaara       define    §azute        azuteds
150700020715     c     *dtaara       define    §datiute      ddatiute
150800020715
150900020715     c                   in(E)     *dtaara
151000020715     c                   if        %Error or rsut = *blanks
151100020715     c                   clear                   tibs34ds
151200020715     c                   call      'TIBS34R'
151300020715     c                   parm                    Tibs34Ds
151400020715     c                   in        *dtaara
151500020715     c                   endif
151600000000     C*
151700000000     C* CARICA LE TABELLE
151800000000     C                   EXSR      CARTAB
151900960115     C*
152000960118     C* MEMORIZZA IN SCHIERA I TRAINI VALIDI DA STAMPARE CON ORDINE PRESCELTO
152100960115     C                   EXSR      ELATLT
152200000000     C*
152300000000     C                   ENDSR
152400000000     O*------------------------------------------------------------------------*
152500960115**   DPAR - DECODIFICHE PARAMETRI
152600960115LUNEDI'             1
152700960115MARTEDI'            2
152800960115MERCOLEDI'          3
152900960115GIOVEDI'            4
153000960115VENERDI'            5
153100960115SABATO              6
153200960115DOMENICA            7
1533009601188
1534009601189
15350096011810
153600960118NUMERO TRAINO       11
153700961022PUNTO OPERATIVO     12
153800960118ORARIO DI PARTENZA  13
153900960118ORARIO DI ARRIVO    14
15400096011815
154100960115**   DIN2 - DECODIFICHE INTESTAZIONE
154200960115¦ARR    LUN    PAR¦   1
154300960115¦ARR    MAR    PAR¦   2
154400960115¦ARR    MER    PAR¦   3
154500960115¦ARR    GIO    PAR¦   4
154600960115¦ARR    VEN    PAR¦   5
154700960115¦ARR    SAB    PAR¦   6
154800960115¦ARR    DOM    PAR¦   7
154900960115¦ARR   TUTTI   PAR¦   8
