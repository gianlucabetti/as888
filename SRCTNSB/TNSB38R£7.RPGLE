000100940912     H*------------------------------------------------------------------------*
000200971105     H* TRASFERIMENTO BOLLE NELLA LIBRERIA DI SALVATAGGIO
000300940912     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500990802     F*---
000600990802     F* BOLLE
000700990802     F*---
000800990802     FTITAS38C  UF   E           K DISK
000900990802     F                                     IGNORE(TITAS000)
001000990802     F                                     IGNORE(TITAS010)
001100990802     FTITASP0F  O  A E           K DISK
001200990802     F                                     RENAME(TITASP00:TITASS00)
001201061121     FTITASS0F  O  A E           K DISK
001202061121     F                                     RENAME(TITASS00:TITASSTO)
001300990802     F*---
001400990802     F* ESTENSIONE BOLLE
001500990802     F*---
001600990802     FTITA730C  UF   E           K DISK
001700990802     FTITA7P0F  O  A E           K DISK
001800990802     F                                     RENAME(TITA7P00:TITA7S00)
001900990802     F*---
002000990802     F* ANAGRAFICHE
002100990802     F*---
002200990802     FTITAA30C  UF   E           K DISK
002300990802     FTITAAP0F  O  A E           K DISK
002400990802     F                                     RENAME(TITAAP00:TITAAS00)
002500990802     F*---
002600990802     F* SEGNACOLLI
002700990802     F*---
002800990802     FTITAT30C  UF   E           K DISK
002900990802     FTITATP0F  O  A E           K DISK
003000990802     F                                     RENAME(TITATP00:TITATS00)
003100990802     F*---
003200990802     F* RIFERIMENTI
003300990802     F*---
003400990802     FTITA430C  UF   E           K DISK
003500990802     FTITA4P0F  O  A E           K DISK
003600990802     F                                     RENAME(TITA4P00:TITA4S00)
003700010903     F*---
003800010903     F* ESTENSIONE DETTAGLI SEGNACOLLI
003900010903     F*---
004000010903     FTITAH30C  UF   E           K DISK
004100010903     FTITAHP0F  O  A E           K DISK
004200010903     F                                     RENAME(TITAHP00:TITAHS00)
004300990802     F*---
004400990802     F* COMPETENZE TESTATA
004500990802     F*---
004600950307     FECCET30C  UF   E           K DISK
004700950307     FECCETP0F  O  A E           K DISK
004800010829     F                                     RENAME(ECCETP00:ECCETS)
004900990802     F*---
005000990802     F* COMPETENZE DETTAGLIO
005100990802     F*---
005200950307     FECCED30C  UF   E           K DISK
005300950307     FECCEDP0F  O  A E           K DISK
005400010829     F                                     RENAME(ECCEDP00:ECCEDS)
005500040220     F*---
005600990802     F* INFRAGRUPPO METRICUBI
005700990802     F*---
005800980325     FECFIM30C  UF   E           K DISK
005900980325     FECFIMP0F  O  A E           K DISK
006000980325     F                                     RENAME(ECFIMP00:ECFIMS00)
006100990802     F*---
006200990802     F* VOCI COMPETENZE TESTATA
006300990802     F*---
006400970903     FECECT01L  UF   E           K DISK
006500970903     FECECT00F  O  A E           K DISK
006600970903     F                                     RENAME(ECECT:ECECTS)
006700990802     F*---
006800990802     F* VOCI COMPETENZE DETTAGLIO
006900990802     F*---
007000970903     FECECD01L  UF   E           K DISK
007100970903     FECECD00F  O  A E           K DISK
007200970903     F                                     RENAME(ECECD:ECECDS)
007300000327     F*---
007400000327     F* EVENTI
007500000327     F*---
007600000327     FFNEVB01L  UF   E           K DISK
007700000327     FFNEVB00F  O  A E           K DISK
007800000327     F                                     RENAME(FNEVB000:FNEVBS)
007900000404     F*---
008000000404     F* GIACENZE
008100000404     F*---
008200050301     Ftigcp51l  UF   E           K DISK
008300050301     Ftigcp00f  O  A E           K DISK
008400050301     F                                     RENAME(tigcp000:tigcpS)
008500000404     F*---
008600000404     F* GIACENZE -ANAGRAFICHE MITTENTE-
008700000404     F*---
008800050301     Ftigap01l  UF   E           K DISK
008900050301     Ftigap00f  O  A E           K DISK
009000050301     F                                     RENAME(TIGAP000:TIGAPS)
009100000404     F*---
009200000404     F* GIACENZE -NOTE-
009300000404     F*---
009400050301     FTIGNP01L  UF   E           K DISK
009500050301     FTIGNP00F  O  A E           K DISK
009600050301     F                                     RENAME(TIGNP000:TIGNPS)
009700040303     F*---
009800040303     F* ESTENSIONE ALLA TESTATA BOLLE
009900040303     F*---
010000040531     FFIAR531C  UF   E           K DISK
010100040303     FFIAR5P0F  O  A E           K DISK
010200040303     F                                     RENAME(FIAR5P00:FIAR5S)
010201060919     F*---
010202060919     Ftitai01l  UF   E           K DISK
010203060919     Ftitai00F  O  A E           K DISK
010204060919     F                                     RENAME(titai000:titaiS)
010205061122     F*---
010206061122     Ffiapd01l  IF   E           K DISK
010207080312     Ftilad01l  IF   E           K DISK
010300000404     F*---
010400000327     F* TABELLE
010500000327     F*---
010600950309     FTABEL00F  UF   E           K DISK    USROPN
010700000327     F*---
010800000327     F* CONTRASSEGNI
010900000327     F*---
011000950309     FTNCSB03L  IF   E           K DISK
011100000327     F*---
011200000327     F* DANNI
011300000327     F*---
011400040715     F***FNDCT02L  IF   E           K DISK
011500040823     F*---
011600040823     Ffnblp01L  IF   E           K DISK    usropn
011700040823     Ffnarb01L  IF   E           K DISK    usropn
011800000327     F*---
011900000327     F* PRINTER FILE
012000000327     F*---
012100931105     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
012200931105     F                                     FORMLEN(66)
012300931105     F                                     FORMOFL(60)
012400940912     L*------------------------------------------------------------------------*
012500040823     d wlib            s             10    inz('FILTRA201 ')
012600040910     d wlibp           s             10    inz('FILTRAPRD ')
012700040823     d comman          s             80
012800040823     d lenght          s             15  5 inz(80)
012801061122     d trcapd          s                   like(apdtip) inz('A')
012802061122     d com04           s              4  0
012900940912     D*------------------------------------------------------------------------*
013000940912     D* SCHIERE
013100940912     D*------------------------------------------------------------------------*
013200040823     d cmd             s             60    dim(01) ctdata perrcd(1)
013300931105     D TES             S             66    DIM(15) CTDATA PERRCD(1)
013400950309     D* DS TABELLA $3 (DATE AVANZAMENTO TRASFERIMENTO BOLLE)
013500950309     D DS$3          E DS
013600950717     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
013700950717     D DS§DAT          DS
013800950717     D  DSSEC                  1      2  0
013900950717     D  DSANN                  3      4  0
014000950717     D  DSMES                  5      6  0
014100950717     D  DSGIO                  7      8  0
014200950717     D  DSAAS                  1      4  0
014300950717     D  DSMGS                  5      8  0
014400950717     D  DSDAT6                 3      8  0
014500950717     D  DSDAT8                 1      8  0
014600950717     D DS§TIM          DS
014700950717     D  DTORA                  1      2  0
014800950717     D  DTMIN                  3      4  0
014900950717     D  DTSCN                  5      6  0
015000950717     D  DTGIO                  7      8  0
015100950717     D  DTMES                  9     10  0
015200950717     D  DTSEC                 11     12  0
015300950717     D  DTANN                 13     14  0
015400950717     D  DTORA6                 1      6  0
015500950717     D  DTTIME                 1     14  0
015600940912     D*------------------------------------------------------------------------*
015700940912     D* INPUT
015800940912     D*------------------------------------------------------------------------*
015900941123     D                 DS
016000941123     D  DSAASI                 1      4  0
016100941123     D  DSMGSI                 5      8  0
016200950309     D  DSDATI                 1      8  0
016300941123     D                 DS
016400941123     D  DSAASF                 1      4  0
016500941123     D  DSMGSF                 5      8  0
016600950309     D  DSDATF                 1      8  0
016700020220     D                 DS
016800020220     D  DSAASFP                1      4  0
016900020220     D  DSMGSFP                5      8  0
017000020220     D  DSDATFP                1      8  0
017100960413     D*-------------------
017200960413     D* DATA SPEDIZIONE
017300960413     D*-------------------
017400960413     D                 DS
017500960413     D  TASAAS                 1      4  0
017600960413     D  TASMGS                 5      8  0
017700960413     D  TASDSP                 1      8  0
017800000327     D*------------------
017900000327     D* DS "XSRDA8" - CONTROLLA DATA (8)
018000000327     D*------------------
018100000327     D WLBDA8          DS                  INZ
018200000327     D  G08DAT                 1      8  0
018300000327     D  G08INV                 9     16  0
018400000327     D  G08ERR                17     17
018500000327     D  G08TGI                18     22  0
018600040715
018700040715     d Fidn12ds      e ds
018800040715     d  skKey                 26    305    dim(20)
018900040715     d  skAnn                306    325    dim(20)
019000040715     d  skDca                326    485    dim(20)
019100040715     d  skFca                486    545  0 dim(20)
019200040715     d  skDch                546    705  0 dim(20)
019300040715     d  skCch                706    745    dim(20)
019400040715
019500940912     D*-------------------
019600931115     D* PARAMETRI LANCIO PGM
019700940912     D*-------------------
019800000327     D PARAM           DS
019900950309     D  PARDTI                 1      8  0                                      DT.INI
020000950309     D  PARDTF                 9     16  0                                      DT.FIN
020100020220     D  PARDFP                17     24  0                                      DT.FIN PT
020200050706     D  PARcop                25     27                                         DT.FIN PT
020300940912     D*-------------------
020400940912     D* ARCHITETTURA
020500940912     D*-------------------
020600931115     D KPJBA         E DS
020700050705     D tnsb34ds      E DS
020800050705     D dtasflo       E DS
020801061121      *
020802061121     D*-------------------
020803061121     D* ANAGRAFICA
020804061121     D*-------------------
020805061121     d TIBS69ds      e ds                  inz
020806061121     d DS_cnaco      e ds                  inz  extname(CNACO00F)
020807061121     d DS_cnind      e ds                  inz  extname(CNIND00F)
020808061121     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
020809061121     d DS_fncls      e ds                  inz  extname(FNCLS00F)
020810061122     D*-------------------
020811061122     D* RIFERIMENTI ALFANUMERICI
020812061122     D*-------------------
020813061122     d DTA4A         e ds
020814061122     D*-------------------
020815061122     D* CODICE FISCALE
020816061122     D*-------------------
020817061122     d DTA4Q         e ds
020900940912     C*------------------------------------------------------------------------*
021000940912     C*?MAIN LINE
021100940912     C*------------------------------------------------------------------------*
021200000000     C*
021300930129     C* ROUTINE INIZIALE
021400930129     C                   EXSR      RUTINZ
021500931105     C*
021600931105     C* STAMPA RIGA DI TESTATA
021700931105     C                   EXCEPT    TESTA
021800930601     C*
021900930601     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
022000930601     C     FINL00        IFEQ      '0'                                          --- 1 -->
022100000000     C*
022200990802     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA SU TITAS
022300950307     C                   EXSR      SETTAS
022400930128     C*
022500990802     C* CICLO FINO A FINE FILE TITAS OPPURE
022600950308     C* RICHIESTA CHIUSURA PGM OPPURE
022700950308     C* FINE PGM PER ERRORE
022800950308     C                   SETOFF                                       98
022900950307     C     FINTAS        DOWEQ     '0'                                          --- 2 -->
023000950308     C     *IN98         ANDEQ     '0'
023100950308     C     FINL00        ANDEQ     '0'
023200080214      * verifico   che non esistano ARB e BLP altrimenti non cancello
023300040823      * e leggo un'altro rekord
023400040823     C     Kbol          Chain     fnblp01l                           33
023500040922     C  N33              ADD       1             NUMblp                         *REC TRASF TITA7
023600040922     C  n33              EXCEPT    ERRblp
023800080214     C     kbol          Chain     fnarb01l                           34
023900080214     C  N34              ADD       1             NUMarb                         *REC TRASF TITA7
024000080214     C  n34              EXCEPT    ERRarb
024001080214     c                   if        not *in33 or not *in34
024100080214     c                   goto      altrorek
024101080214     c                   end
024200950307     C*
024300001019     C                   WRITE     TITASS00                             99      *SCRIVO TITASS
024400001019     C  N99              DELETE    TITASP00                             99      *CANCELLO TITASP
024500950307     C   99              EXCEPT    ERRTAS
024600950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
024700990802     C  N99              ADD       1             NUMTAS                         *REC TRASF TITAS
024800050705     c* se bolla ha immagine di laguna scrivo record sul file TILAD00F
024900050705     c                   if        not *in99
025000050705     c                   movel     tasflo        dtasflo
025100050705     c                   if        §floiml <> ' '
025200050705     c                   exsr      srlaguna
025300050705     c                   end
025400050705     c                   end
025500950307     C*
025600990802     C* SE ESISTE TITA7 TRASFERISCO ANCHE QUELLO
025700950307     C     TASSV3        IFNE      *BLANKS                                      --- 3 -->
025800990802     C                   EXSR      SETTA7                                       *PRIMO TITA7
025900950307     C     FINTA7        DOWEQ     '0'                                          --- 4 -->
026000990802     C                   WRITE     TITA7S00                             99      *SCRIVO TITA7S
026100001019     C  N99              DELETE    TITA730C                             99      *CANCELLO TITA730C
026200950307     C   99              EXCEPT    ERRTA7
026300950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
026400990802     C  N99              ADD       1             NUMTA7                         *REC TRASF TITA7
026500990802     C                   EXSR      LETTA7                                       *SUCCESSIVO TITA7
026600950307     C                   ENDDO                                                  <-- 4 ---
026700950307     C                   ENDIF                                                  <-- 3 ---
026800990802     C*
026900990802     C* TRASFERISCE TITAA - ANAGRAFICHE
027000990802     C                   EXSR      SETTAA                                       *PRIMO TITAA
027100990802DO  3C     FINTAA        DOWEQ     '0'
027200990802     C                   WRITE     TITAAS00                             99      *SCRIVO TITAAS
027300001019     C  N99              DELETE    TITAA30C                             99      *CANCELLO TITAA30C
027400990802     C   99              EXCEPT    ERRTAA
027500990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
027600990802     C  N99              ADD       1             NUMTAA                         *REC TRASF TITAA
027700990802     C                   EXSR      LETTAA                                       *SUCCESSIVO TITAA
027800990802E   3C                   ENDDO
027801061121     C* verifico se scritto i dati anagrafici del mittente altrimenti aggancio anagrafica
027802061121     c                   if        tasrsm = *blanks
027803061121     c                   exsr      recana
027804061121     c                   endif
027900990802     C*
028000990802     C* TRASFERISCE TITAT - SEGNACOLLI
028100990802     C                   EXSR      SETTAT                                       *PRIMO TITAT
028200990802DO  3C     FINTAT        DOWEQ     '0'
028300990802     C                   WRITE     TITATS00                             99      *SCRIVO TITATS
028400001019     C  N99              DELETE    TITAT30C                             99      *CANCELLO TITAT30C
028500990802     C   99              EXCEPT    ERRTAT
028600990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
028700990802     C  N99              ADD       1             NUMTAT                         *REC TRASF TITAT
028800990802     C                   EXSR      LETTAT                                       *SUCCESSIVO TITAT
028900990802E   3C                   ENDDO
029000990802     C*
029100990802     C* TRASFERISCE TITA4 - RIFERIMENTI
029200990802     C                   EXSR      SETTA4                                       *PRIMO TITA4
029300990802DO  3C     FINTA4        DOWEQ     '0'
029400990802     C                   WRITE     TITA4S00                             99      *SCRIVO TITA4S
029500001019     C  N99              DELETE    TITA430C                             99      *CANCELLO TITA430C
029600990802     C   99              EXCEPT    ERRTA4
029700990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
029800990802     C  N99              ADD       1             NUMTA4                         *REC TRASF TITA4
029900990802     C                   EXSR      LETTA4                                       *SUCCESSIVO TITA4
030000990802E   3C                   ENDDO
030100010903     C*
030200010903     C* TRASFERISCE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
030300010903     C                   EXSR      SETTAH                                       *PRIMO TITAH
030400010903DO  3C     FINTAH        DOWEQ     '0'
030500010903     C                   WRITE     TITAHS00                             99      *SCRIVO TITAHS
030600010903     C  N99              DELETE    TITAH30C                             99      *CANCELLO TITAH30C
030700010903     C   99              EXCEPT    ERRTAH
030800010903     C   99              MOVEL     '1'           FINL00                         *FINE PGM
030900010903     C  N99              ADD       1             NUMTAH                         *REC TRASF TITAH
031000010903     C                   EXSR      LETTAH                                       *SUCCESSIVO TITAH
031100010903E   3C                   ENDDO
031200950307     C*
031300950307     C* SE ESISTE ECCET LO TRASFERISCE
031400971008     C                   Z-ADD     999           MINNBC                         MIMIMO BLOCCO
031500950307     C                   Z-ADD     TASAAS        CETAAS                         ANNO SPEDIZIONE
031600950307     C                   Z-ADD     TASLNP        CETLNP                         LINEA DI PARTENZA
031700950307     C                   Z-ADD     TASNRS        CETNRS                         NUMERO SERIE
031800950307     C                   Z-ADD     TASNSP        CETNSP                         NUMERO SPEDIZIONE
031900950307     C                   MOVEL     TASTBL        CETTBL                         TIPO BOLLA
032000950307     C     KEYCET        CHAIN     ECCET30C                           99
032100950307     C     *IN99         IFEQ      *OFF                                         --- 3 --> *TROVATO
032200970903     C*
032300970903     C* MEMORIZZA IL NUMERO BLOCCO COMPETENZE TESTATA PIU' PICCOLO
032400971008IF  4C     CETNBC        IFGT      *ZEROS
032500971008IF  5C     CETNBC        IFLT      MINNBC
032600970903     C                   Z-ADD     CETNBC        MINNBC
032700971008E   5C                   ENDIF
032800971008E   4C                   ENDIF
032900970903     C*
033000950309     C                   WRITE     ECCETS                               99      *SCRIVO ECCETS
033100001019     C  N99              DELETE    ECCET30C                             99      *CANCELLO ECCET30C
033200950307     C   99              EXCEPT    ERRCET
033300950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
033400950308     C  N99              ADD       1             NUMCET                         *REC TRASF ECCET
033500950307     C*
033600950307     C* SE ESISTE ECCED TRASFERISCO ANCHE QUELLO
033700950307     C     CETDET        IFEQ      'S'                                          --- 4 -->
033800950307     C                   EXSR      SETCED                                       *PRIMO ECCED
033900950307     C     FINCED        DOWEQ     '0'                                          --- 5 -->
034000950309     C                   WRITE     ECCEDS                               99      *SCRIVO ECCEDS
034100001020     C  N99              DELETE    ECCED30C                             99      *CANCELLO ECCED30C
034200950307     C   99              EXCEPT    ERRCED
034300950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
034400950308     C  N99              ADD       1             NUMCED                         *REC TRASF ECCED
034500950307     C                   EXSR      LETCED                                       *SUCCESSIVO ECCED
034600950307     C                   ENDDO                                                  <-- 5 ---
034700950307     C                   ENDIF                                                  <-- 4 ---
034800950307     C                   ENDIF                                                  <-- 3 ---
034900980325     C*
035000980325     C* SE ESISTE ECFIM LO TRASFERISCE
035100980325     C                   Z-ADD     TASAAS        FIMAAS                         ANNO SPEDIZIONE
035200980325     C                   Z-ADD     TASLNP        FIMLNP                         LINEA DI PARTENZA
035300980325     C                   Z-ADD     TASNRS        FIMNRS                         NUMERO SERIE
035400980325     C                   Z-ADD     TASNSP        FIMNSP                         NUMERO SPEDIZIONE
035500980325     C                   MOVEL     TASTBL        FIMTBL                         TIPO BOLLA
035600001019     C     KEYFIM        CHAIN     ECFIM30C                           99
035700980325     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
035800980325     C                   WRITE     ECFIMS00                             99      *SCRIVE
035900001019     C  N99              DELETE    ECFIM30C                             99      *CANCELLA
036000980325     C   99              EXCEPT    ERRFIM
036100980325     C   99              MOVEL     '1'           FINL00                         *FINE PGM
036200980325     C  N99              ADD       1             NUMFIM                         *REC TRASF ECCET
036300011127     C     KEYFIM        READE     ECFIM30C                               99
036400980325     C                   ENDDO                                                  <-- 3 ---
036500000327     C*
036600000327     C* SE ESISTE FNEVB LO TRASFERISCE
036700000327     C                   Z-ADD     TASAAS        EVBAAS                         ANNO SPEDIZIONE
036800000327     C                   Z-ADD     TASLNP        EVBLNP                         LINEA DI PARTENZA
036900000327     C                   Z-ADD     TASNRS        EVBNRS                         NUMERO SERIE
037000000327     C                   Z-ADD     TASNSP        EVBNSP                         NUMERO SPEDIZIONE
037100001019     C     KEYEVB        CHAIN     FNEVB01L                           99
037200000327     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
037300000327     C                   WRITE     FNEVBS                               99      *SCRIVE
037400001019     C  N99              DELETE    FNEVB01L                             99      *CANCELLA
037500000327     C   99              EXCEPT    ERREVB
037600000327     C   99              MOVEL     '1'           FINL00                         *FINE PGM
037700000327     C  N99              ADD       1             NUMEVB                         *REC TRASF FNEVB
037800000327     C     KEYEVB        READE     FNEVB01L                               99
037900000327     C                   ENDDO                                                  <-- 3 ---
038000000404     C*
038100050301     C* SE ESISTE TIGCP LO TRASFERISCE
038200001018     C     TASFGC        IFEQ      'S'
038300000404     C                   Z-ADD     TASAAS        KCPAAS                         ANNO SPEDIZIONE
038400000404     C                   Z-ADD     TASLNP        KCPLNP                         LINEA DI PARTENZA
038500000404     C                   Z-ADD     TASNRS        KCPNRS                         NUMERO SERIE
038600000404     C                   Z-ADD     TASNSP        KCPNSP                         NUMERO SPEDIZIONE
038700050301     C     KE2GCP        CHAIN     TIGCP51L                           99
038800001018     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
038900050303     c     gcpsede       ifeq      'S'
039000050301     C                   WRITE     TIGCPS                               99      *SCRIVE
039100050301     C  N99              DELETE    TIGCP51L                             99      *CANCELLA
039200000404     C   99              EXCEPT    ERRGCP
039300000404     C   99              MOVEL     '1'           FINL00                         *FINE PGM
039400000404     C  N99              ADD       1             NUMGCP                         *REC TRASF FNEVB
039500000404     C*
039600050301     C* SE ESISTE TIGAP LO TRASFERISCE
039700000404     C                   Z-ADD     GCPAAS        KAPAAS                         ANNO SPEDIZIONE
039800000404     C                   Z-ADD     GCPLNP        KAPLNP                         LINEA DI PARTENZA
039900000404     C                   Z-ADD     GCPNRS        KAPNRS                         NUMERO SERIE
040000000404     C                   Z-ADD     GCPNSP        KAPNSP                         NUMERO SPEDIZIONE
040100050301     C     KEYGAP        CHAIN     TIGAP01L                           99
040200001018     C     *IN99         DOWEQ     *OFF
040300050301     C                   WRITE     TIGAPS                               99      *SCRIVE
040400050301     C  N99              DELETE    TIGAP01L                             99      *CANCELLA
040500001017     C   99              EXCEPT    ERRGAP
040600001017     C   99              MOVEL     '1'           FINL00                         *FINE PGM
040700001017     C  N99              ADD       1             NUMGAP                         *REC TRASF FNEVB
040800050301     C     KEYGAP        READE     TIGAP01L                               99
040900001018     C                   ENDDO
041000000404     C*
041100050301     C* SE ESISTE TIGNP LO TRASFERISCE
041200000404     C                   Z-ADD     GCPAGC        KNPAGC                         ANNO APERTURA
041300000404     C                   Z-ADD     GCPFGC        KNPFGC                         FILIALE APERTURA
041400000404     C                   Z-ADD     GCPNGC        KNPNGC                         NUMERO GIACENZA
041500050301     C     KEYGNP        CHAIN     TIGNP01L                           99
041600001018     C     *IN99         DOWEQ     *OFF
041700050301     C                   WRITE     TIGNPS                               99      *SCRIVE
041800050301     C  N99              DELETE    TIGNP01L                             99      *CANCELLA
041900001017     C   99              EXCEPT    ERRGNP
042000001017     C   99              MOVEL     '1'           FINL00                         *FINE PGM
042100001017     C  N99              ADD       1             NUMGNP                         *REC TRASF FNEVB
042200050301     C     KEYGNP        READE     TIGNP01L                               99
042300001018     C                   ENDDO
042400050303     c                   else
042500050303     C                   EXCEPT    ERRGCP1
042600060707     C*                  MOVEL     '1'           FINL00                         *FINE PGM
042700050303     C                   ENDIF
042800000404     C*
042900000404     C* LETTURA SUCCESSIVA GIACENZE
043000050301     C     KE2GCP        READE     TIGCP51L                               99
043100001018     C                   ENDDO
043200001018     C                   ENDIF
043300040303     C*
043400040303     C* SE ESISTE FIAR5 LO TRASFERISCE
043500040303     C                   Z-ADD     TASAAS        AR5AAS                         ANNO SPEDIZIONE
043600040303     C                   Z-ADD     TASLNP        AR5LNP                         LINEA DI PARTENZA
043700040303     C                   Z-ADD     TASNRS        AR5NRS                         NUMERO SERIE
043800040303     C                   Z-ADD     TASNSP        AR5NSP                         NUMERO SPEDIZIONE
043900040531     C     KEYAR5        CHAIN     FIAR531C                           99
044000040303     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
044100040303     C                   WRITE     FIAR5S                               99      *SCRIVE
044200040531     C  N99              DELETE    FIAR531C                             99      *CANCELLA
044300040303     C   99              EXCEPT    ERRAR5
044400040303     C   99              MOVEL     '1'           FINL00                         *FINE PGM
044500040303     C  N99              ADD       1             NUMAR5                         *REC TRASF FNEVB
044600040531     C     KEYAR5        READE     FIAR531C                               99
044700040303     C                   ENDDO                                                  <-- 3 ---
044701060919     C*
044702060919     C* SE ESISTE TITAI LO TRASFERISCE
044703060919     C                   Z-ADD     TASAAS        taiAAS                         ANNO SPEDIZIONE
044704060919     C                   Z-ADD     TASLNP        taiLNP                         LINEA DI PARTENZA
044705060919     C                   Z-ADD     TASNRS        taiNRS                         NUMERO SERIE
044706060919     C                   Z-ADD     TASNSP        taiNSP                         NUMERO SPEDIZIONE
044707060919     C                   movel     TASTBL        taiTBL                         NUMERO SPEDIZIONE
044708060919     C     KEYtai        CHAIN     titai01l                           99
044709060919     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
044710060919     C                   WRITE     titaiS                               99      *SCRIVE
044711060919     C  N99              DELETE    titai01l                             99      *CANCELLA
044712060919     C   99              EXCEPT    ERRtai
044713060919     C   99              MOVEL     '1'           FINL00                         *FINE PGM
044714060919     C  N99              ADD       1             NUMtai                         *REC TRASF FNEVB
044715060919     C     KEYtai        READE     titai01l                               99
044716060919     C                   ENDDO                                                  <-- 3 ---
044717061214      * decodifico alcuni dati per il nuovo storico  e lo scrivo se non bolla Poste
044718061214     c                   If        tastsp <> 'P'
044719061122     c                   EXSR      wrttassto
044720061214     c                   endif
044721061122
044800000000     C*
044900980325     C* LETTURA SUCCESSIVA BOLLE
045000040823     c     altrorek      tag
045100040823
045101060707     c                   if        finl00 = '0'
045200990802     C                   EXSR      LETTAS                                       *SUCCESSIVO TITAS
045201060707     c                   end
045300950308     C                   SHTDN                                        98        *CONTR.CHIUSURA PGM
045400930128     C*
045500950307     C                   ENDDO                                                  <-- 2 ---
045600950307     C                   ENDIF                                                  <-- 1 ---
045700970903     C*
045800970903     C* PULISCE I CODICI COMPETENZA -TESTATA E DETTAGLIO- CON NUMERO BLOCCO
045900970903     C* MINORE DI QUELLO PIU' PICCOLO CON CUI SONO STATE CALCOLATE LE BOLLE
046000971008IF  1C     MINNBC        IFNE      999
046100970903     C     *LOVAL        SETLL     ECECT01L
046200970903     C                   READ      ECECT01L                               97
046300971008DO  2C     *IN97         DOWEQ     *OFF
046400971008IF  3C     ECTATB        IFEQ      *BLANKS
046500971008IF  4C     ECTNBC        IFLT      MINNBC
046600970903     C                   MOVEL     'A'           ECTATB
046700970903     C                   MOVEL     *BLANKS       ECTFTR
046800970903     C                   EXCEPT    AGGECT                                       *AGGIORNAMENTO CAMPI
046900970903     C                   WRITE     ECECTS                               97      *SCRIVO ECECTS
047000970903     C   97              EXCEPT    ERRECT                                       *SCRIVE ERRORE
047100970903     C   97              MOVEL     '1'           FINL00                         *FINE PGM
047200970903     C  N97              ADD       1             NUMECT                         *REC TRASF ECECT
047300971008E   4C                   ENDIF
047400971008E   3C                   ENDIF
047500970903     C                   READ      ECECT01L                               97
047600971008E   2C                   ENDDO
047700950309     C*
047800970903     C     *LOVAL        SETLL     ECECD01L
047900970903     C                   READ      ECECD01L                               97
048000971008DO  2C     *IN97         DOWEQ     *OFF
048100971008IF  3C     ECDATB        IFEQ      *BLANKS
048200971008IF  4C     ECDNBC        IFLT      MINNBC
048300970903     C                   MOVEL     'A'           ECDATB
048400970903     C                   MOVEL     *BLANKS       ECDFTR
048500970903     C                   EXCEPT    AGGECD                                       *AGGIORNAMENTO CAMPI
048600970903     C                   WRITE     ECECDS                               97      *SCRIVO ECECDS
048700970903     C   97              EXCEPT    ERRECD                                       *SCRIVE ERRORE
048800970903     C   97              MOVEL     '1'           FINL00                         *FINE PGM
048900970903     C  N97              ADD       1             NUMECD                         *REC TRASF ECECD
049000971008E   4C                   ENDIF
049100971008E   3C                   ENDIF
049200970903     C                   READ      ECECD01L                               97
049300971008E   2C                   ENDDO
049400971008E   1C                   ENDIF
049500970903     C*
049600950309     C* AGGIORNA LA TABELLA CON L'ULTIMA DATA DI TRASFERIM. (SOLO SE TUTTO OK)
049700950309     C     *IN98         IFEQ      '0'
049800950309     C     FINL00        ANDEQ     '0'
049900950309     C                   EXSR      AGGTAB
050000950309     C                   ENDIF
050100931105     C*
050200931105     C* STAMPA RIGA DI FINE STAMPA
050300950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
050400931105     C                   EXCEPT    STAFIN
050500050705     c* chiudo il pgm di archiviazione Laguna
050600050705     C                   CLEAR                   Tnsb34ds
050700050705     C                   MOVEL     'C'           sb34tla
050800050705     C                   CALL      'TNSB34R'
050900050705     C                   PARM                    TNSB34ds
051000000000     C*
051100000000     C                   SETON                                        LR
051200940912     C*------------------------------------------------------------------------*
051300990802     C*? SETTAS - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAS
051400940912     C*------------------------------------------------------------------------*
051500950307     C     SETTAS        BEGSR
051600931115     C*
051700931115     C* AZZERO CONTATORE RECORD LETTI
051800950307     C                   MOVEL     '0'           FINTAS            1
051900930208     C*
052000930316     C* POSIZIONAMENTO E PRIMA LETTURA
052100941123     C                   Z-ADD     PARDTI        DSDATI
052200941123     C                   Z-ADD     PARDTF        DSDATF
052300020220     C                   Z-ADD     PARDFP        DSDATFP
052400941123     C                   Z-ADD     DSAASI        TASAAS
052500941123     C                   Z-ADD     DSMGSI        TASMGS
052600990802     C     KEYTAS        SETLL     TITAS38C                           99        *FINE-ARCHIV
052700930625     C*
052800930625     C     *IN99         IFEQ      '1'
052900950307     C                   MOVEL     '1'           FINTAS
053000930625     C                   ELSE
053100950307     C                   EXSR      LETTAS
053200001020     C                   ENDIF
053300930208     C*
053400930208     C                   ENDSR
053500940912     C*------------------------------------------------------------------------*
053600990802     C*? LETTAS - LETTURA PROSSIMO RECORD TITAS
053700940912     C*------------------------------------------------------------------------*
053800950307     C     LETTAS        BEGSR
053900930625     C*
054000930625     C* LEGGO FINO A:
054100930625     C*     - FINE FILE O SUPERATO LIMITE FINALE
054200930625     C*     - TROVATO RECORD VALIDO
054300930625     C                   MOVEL     'N'           WRECOK
054400950307     C     FINTAS        DOWEQ     '0'                                          --- 1 -->
054500930625     C     WRECOK        ANDEQ     'N'
054501061121      * pulisco lo storico
054502061121     c                   clear                   titassto
054600930208     C*
054700930316     C* LETTURE SUCCESSIVE ALLA PRIMA
054800990802     C                   READ      TITAS38C                               99
054900960413     C*
055000960413     C     *IN99         IFEQ      '1'                                          *FINE FILE
055100020220     C     TASDSP        ORGT      DSDATF                                       *SUPERATA DATA FINE
055200950307     C                   MOVEL     '1'           FINTAS
055300930625     C                   ELSE
055400950309     C                   EXSR      CHKTAS                                       *CONTR.VALIDITA REC.
055500001019     C                   ENDIF
055600930625     C*
055700001019     C                   ENDDO                                                  <-- 1 ---
055800930208     C*
055900930208     C                   ENDSR
056000950307     C*------------------------------------------------------------------------*
056100990802     C*? SETTA7 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA7
056200950307     C*------------------------------------------------------------------------*
056300950307     C     SETTA7        BEGSR
056400950307     C*
056500950307     C* AZZERO CONTATORE RECORD LETTI
056600950307     C                   MOVEL     '0'           FINTA7            1
056700950307     C*
056800950307     C* POSIZIONAMENTO E PRIMA LETTURA
056900950307     C                   Z-ADD     TASAAS        TA7AAS
057000950307     C                   Z-ADD     TASLNP        TA7LNP
057100950307     C                   Z-ADD     TASNRS        TA7NRS
057200950307     C                   Z-ADD     TASNSP        TA7NSP
057300950307     C                   MOVEL     TASTBL        TA7TBL
057400001019     C*
057500001019     C     KEYTA7        CHAIN     TITA730C                           99        *FINE-ARCHIV
057600950307     C     *IN99         IFEQ      '1'
057700950307     C                   MOVEL     '1'           FINTA7
057800001019     C                   ENDIF
057900950307     C*
058000950307     C                   ENDSR
058100950307     C*------------------------------------------------------------------------*
058200990802     C*? LETTA7 - LETTURA PROSSIMO RECORD TITA7
058300950307     C*------------------------------------------------------------------------*
058400001019     C     LETTA7        BEGSR
058500001020     C*
058600001020     C* LETTURE SUCCESSIVE ALLA PRIMA
058700010124     C                   READ      TITA730C                               99
058800950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
058900010124     C     TA7AAS        ORNE      TASAAS
059000010124     C     TA7LNP        ORNE      TASLNP
059100010124     C     TA7NRS        ORNE      TASNRS
059200010124     C     TA7NSP        ORNE      TASNSP
059300010124     C     TA7TBL        ORNE      TASTBL
059400950307     C                   MOVEL     '1'           FINTA7
059500001019     C                   ENDIF
059600950307     C*
059700950307     C                   ENDSR
059800990802     C*------------------------------------------------------------------------*
059900990802     C*? SETTAA - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAA - ANAGRAFICHE
060000990802     C*------------------------------------------------------------------------*
060100990802     C     SETTAA        BEGSR
060200990802     C*
060300990802     C* AZZERO CONTATORE RECORD LETTI
060400990802     C                   MOVEL     '0'           FINTAA            1
060500990802     C*
060600990802     C* POSIZIONAMENTO E PRIMA LETTURA
060700990802     C                   Z-ADD     TASAAS        TAAAAS
060800990802     C                   Z-ADD     TASLNP        TAALNP
060900990802     C                   Z-ADD     TASNRS        TAANRS
061000990802     C                   Z-ADD     TASNSP        TAANSP
061100001019     C     KEYTAA        CHAIN     TITAA30C                           99        *FINE-ARCHIV
061200990802     C     *IN99         IFEQ      '1'
061300990802     C                   MOVEL     '1'           FINTAA
061301061121      * recupero dati anagrafici del mittente
061302061121     c                   else
061303061121     c                   exsr      rectaa
061400001019     C                   ENDIF
061500990802     C*
061600990802     C                   ENDSR
061700990802     C*------------------------------------------------------------------------*
061800990802     C*? LETTAA - LETTURA PROSSIMO RECORD TITAA - ANAGRAFICHE
061900990802     C*------------------------------------------------------------------------*
062000990802     C     LETTAA        BEGSR
062100010124     C*
062200010124     C                   READ      TITAA30C                               99
062300990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
062400010130     C     TAAAAS        ORNE      TASAAS
062500010130     C     TAALNP        ORNE      TASLNP
062600010130     C     TAANRS        ORNE      TASNRS
062700010130     C     TAANSP        ORNE      TASNSP
062800990802     C                   MOVEL     '1'           FINTAA
062900001019     C                   ENDIF
062901061121      * se letto
062902061123     c                   if        fintaa = '0'
062903061121     c                   exsr      rectaa
062904061121     c                   endif
063000990802     C*
063100990802     C                   ENDSR
063101061121     C*------------------------------------------------------------------------*
063102061121     C*? RECTAA - RECUPERO DATI ANAGRAFICI MITTENTE
063103061121     C*------------------------------------------------------------------------*
063104061121     C     RECTAA        BEGSR
063105061121      * dati anagrafici mittente
063106061121     c                   IF        taatrc = 'M'
063107061121     c                   eval      tasrsm = taarsc
063108061121     c                   eval      tasinm = taaind
063109061121     c                   eval      tascam = taacap
063110061121     c                   eval      taslom = taaloc
063111061121     c                   eval      tasprm = taaprv
063112061121     c                   eval      tasnzm = taanaz
063113061122      * iva  per i franchi non coidficati
063114061122     c                   if        %subst(tastbl:1:1) = 'F'
063115061122     c                   eval      tasiva = taacpi
063116061122     c                   endif
063117061122
063118061121     c                   endif
063119061121
063120061121     c                   endsr
063121061121     C*------------------------------------------------------------------------*
063122061121     C*? RECANA - RECUPERO DA ANAGRAFICO CLIENTE
063123061121     C*------------------------------------------------------------------------*
063124061121     C     RECANA        BEGSR
063125061121     C                   clear                   ds_CNACO
063126061121     C                   clear                   ds_CNIND
063127061121     C                   clear                   ds_CNCLP
063128061121     C                   clear                   ds_FNCLS
063129061121     c                   z-add     TASccm        I69kac
063130061121     c                   z-add     TASccm        I69kin
063131061121     c                   call      'TIBS69R'
063132061121     c                   parm                    TIBS69ds
063133061121     c                   parm                    ds_CNACO
063134061121     c                   parm                    ds_CNIND
063135061121     c                   parm                    ds_CNCLP
063136061121     c                   parm                    ds_FNCLS
063137061122if  3c                   if        O69err <> *on
063138061122     c                   movel     ACOrag        TASrsm
063139061122     c                   movel     INDvia        TASinm
063140061122     c                   movel     INDcap        TAScam
063141061122     c                   movel     INDcit        TASlom
063142061122     c                   movel     INDprv        TASprm
063143061122     c                   movel     INDsta        TASnzm
063144061122e   3c                   endif
063145061121
063146061121     c                   endsr
063147061122     C*------------------------------------------------------------------------*
063148061122     C*? RECIVA - RECUPERO DA ANAGRAFICO CLIENTE IVA E/O CODICE FISCALE
063149061122     C*------------------------------------------------------------------------*
063150061122     C     RECIVA        BEGSR
063151061122     C                   clear                   ds_CNACO
063152061122     C                   clear                   ds_CNIND
063153061122     C                   clear                   ds_CNCLP
063154061122     C                   clear                   ds_FNCLS
063156061122     c                   z-add     TASKSC        I69kin
063157061122     c                   call      'TIBS69R'
063158061122     c                   parm                    TIBS69ds
063159061122     c                   parm                    ds_CNACO
063160061122     c                   parm                    ds_CNIND
063161061122     c                   parm                    ds_CNCLP
063162061122     c                   parm                    ds_FNCLS
063171061122
063172061122     c                   endsr
063200990802     C*------------------------------------------------------------------------*
063300990802     C*? SETTAT - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAT - SEGNACOLLI
063400990802     C*------------------------------------------------------------------------*
063500990802     C     SETTAT        BEGSR
063600990802     C*
063700990802     C* AZZERO CONTATORE RECORD LETTI
063800990802     C                   MOVEL     '0'           FINTAT            1
063900990802     C*
064000990802     C* POSIZIONAMENTO E PRIMA LETTURA
064100990802     C                   Z-ADD     TASAAS        TATAAS
064200990802     C                   Z-ADD     TASLNP        TATLNP
064300990802     C                   Z-ADD     TASNRS        TATNRS
064400990802     C                   Z-ADD     TASNSP        TATNSP
064500001019     C     KEYTAT        CHAIN     TITAT30C                           99        *FINE-ARCHIV
064600990802     C     *IN99         IFEQ      '1'
064700990802     C                   MOVEL     '1'           FINTAT
064800001019     C                   ENDIF
064900990802     C*
065000990802     C                   ENDSR
065100990802     C*------------------------------------------------------------------------*
065200990802     C*? LETTAT - LETTURA PROSSIMO RECORD TITAT - SEGNACOLLI
065300990802     C*------------------------------------------------------------------------*
065400990802     C     LETTAT        BEGSR
065500010124     C*
065600010124     C                   READ      TITAT30C                               99
065700990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
065800010124     C     TATAAS        ORNE      TASAAS
065900010124     C     TATLNP        ORNE      TASLNP
066000010124     C     TATNRS        ORNE      TASNRS
066100010124     C     TATNSP        ORNE      TASNSP
066200990802     C                   MOVEL     '1'           FINTAT
066300001019     C                   ENDIF
066400990802     C*
066500990802     C                   ENDSR
066600990802     C*------------------------------------------------------------------------*
066700990802     C*? SETTA4 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA4 - RIFERIMENTI
066800990802     C*------------------------------------------------------------------------*
066900990802     C     SETTA4        BEGSR
067000990802     C*
067100990802     C* AZZERO CONTATORE RECORD LETTI
067200990802     C                   MOVEL     '0'           FINTA4            1
067300990802     C*
067400990802     C* POSIZIONAMENTO E PRIMA LETTURA
067500990802     C                   Z-ADD     TASAAS        TA4AAS
067600990802     C                   Z-ADD     TASLNP        TA4LNP
067700990802     C                   Z-ADD     TASNRS        TA4NRS
067800990802     C                   Z-ADD     TASNSP        TA4NSP
067900001019     C     KEYTA4        CHAIN     TITA430C                           99        *FINE-ARCHIV
068000990802     C     *IN99         IFEQ      '1'
068100990802     C                   MOVEL     '1'           FINTA4
068101061122      * recupero riferimento mittente alfanumerico
068102061122     c                   else
068103061122     c                   exsr      recta4
068200001019     C                   ENDIF
068300990802     C*
068400990802     C                   ENDSR
068500990802     C*------------------------------------------------------------------------*
068600990802     C*? LETTA4 - LETTURA PROSSIMO RECORD TITA4 - RIFERIMENTI
068700990802     C*------------------------------------------------------------------------*
068800990802     C     LETTA4        BEGSR
068900010124     C*
069000010124     C                   READ      TITA430C                               99
069100990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
069200010124     C     TA4AAS        ORNE      TASAAS
069300010124     C     TA4LNP        ORNE      TASLNP
069400010124     C     TA4NRS        ORNE      TASNRS
069500010124     C     TA4NSP        ORNE      TASNSP
069600990802     C                   MOVEL     '1'           FINTA4
069700001019     C                   ENDIF
069701061122      * se letto
069702061123     c                   if        finta4 = '0'
069703061122     c                   exsr      recta4
069704061122     c                   endif
069800990802     C*
069900990802     C                   ENDSR
069901061122     C*------------------------------------------------------------------------*
069902061122     C*? RECTA4 - RECUPERO RIFERIMENTO MITTENTE ALFANUMERICO / CODICE FISCALE
069903061122     C*------------------------------------------------------------------------*
069904061122     C     RECTA4        BEGSR
069905061122      * riferimento mittente alfanumerico
069906061122     c                   IF        ta4trc = 'A'
069907061122     c                   movel     ta4not        dta4a
069908061122     c                   movel     §ta4aRMA      Tasrma
069915061122     c                   endif
069916061122      * CODICE FISCALE
069917061122     c                   IF        ta4trc = 'Q'
069918061122     c                   movel     ta4not        dta4q
069919061122      * VERIFICO SE FRANCO O ASSEGNATO
069920061122     c                   If        %subst(tastbl:1:1) = 'F'
069921061122     c                   movel     §TA4QCFM      Tascdf
069922061122     c                   endif
069923061122     c                   If        %subst(tastbl:1:1) = 'A'
069924061122     c                   movel     §TA4QCFD      Tascdf
069925061122     c                   endif
069926061122     c                   endif
069927061122
069928061122     c                   endsr
070000010903     C*------------------------------------------------------------------------*
070100010903     C*? SETTAH - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
070200010903     C*------------------------------------------------------------------------*
070300010903     C     SETTAH        BEGSR
070400010903     C*
070500010903     C* AZZERO CONTATORE RECORD LETTI
070600010903     C                   MOVEL     '0'           FINTAH            1
070700010903     C*
070800010903     C* POSIZIONAMENTO E PRIMA LETTURA
070900010903     C                   Z-ADD     TASAAS        TAHAAS
071000010903     C                   Z-ADD     TASLNP        TAHLNP
071100010903     C                   Z-ADD     TASNRS        TAHNRS
071200010903     C                   Z-ADD     TASNSP        TAHNSP
071300010903     C     KEYTAH        CHAIN     TITAH30C                           99        *FINE-ARCHIV
071400010903     C     *IN99         IFEQ      '1'
071500010903     C                   MOVEL     '1'           FINTAH
071600010903     C                   ENDIF
071700010903     C*
071800010903     C                   ENDSR
071900010903     C*------------------------------------------------------------------------*
072000010903     C*? LETTAH - LETTURA PROSSIMO RECORD TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
072100010903     C*------------------------------------------------------------------------*
072200010903     C     LETTAH        BEGSR
072300010903     C*
072400010903     C                   READ      TITAH30C                               99
072500010903     C     *IN99         IFEQ      '1'                                          *FINE FILE
072600010903     C     TAHAAS        ORNE      TASAAS
072700010903     C     TAHLNP        ORNE      TASLNP
072800010903     C     TAHNRS        ORNE      TASNRS
072900010903     C     TAHNSP        ORNE      TASNSP
073000010903     C                   MOVEL     '1'           FINTAH
073100010903     C                   ENDIF
073200010903     C*
073300010903     C                   ENDSR
073400950307     C*------------------------------------------------------------------------*
073500950307     C*? SETCED - POSIZIONAMENTO E PRIMA LETTURA DEL FILE ECCED
073600950307     C*------------------------------------------------------------------------*
073700950307     C     SETCED        BEGSR
073800950307     C*
073900950307     C* AZZERO CONTATORE RECORD LETTI
074000950307     C                   MOVEL     '0'           FINCED            1
074100950307     C*
074200950307     C* POSIZIONAMENTO E PRIMA LETTURA
074300950307     C                   Z-ADD     TASAAS        CEDAAS
074400950307     C                   Z-ADD     TASLNP        CEDLNP
074500950307     C                   Z-ADD     TASNRS        CEDNRS
074600950307     C                   Z-ADD     TASNSP        CEDNSP
074700950307     C                   MOVEL     TASTBL        CEDTBL
074800001019     C     KEYCED        CHAIN     ECCED30C                           99        *FINE-ARCHIV
074900950307     C     *IN99         IFEQ      '1'
075000950307     C                   MOVEL     '1'           FINCED
075100001019     C                   ENDIF
075200950307     C*
075300950307     C                   ENDSR
075400950307     C*------------------------------------------------------------------------*
075500950307     C*? LETCED - LETTURA PROSSIMO RECORD ECCED
075600950307     C*------------------------------------------------------------------------*
075700950307     C     LETCED        BEGSR
075800950307     C*
075900010124     C                   READ      ECCED30C                               99
076000950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
076100010124     C     CEDAAS        ORNE      TASAAS
076200010124     C     CEDLNP        ORNE      TASLNP
076300010124     C     CEDNRS        ORNE      TASNRS
076400010124     C     CEDNSP        ORNE      TASNSP
076500010124     C     CEDTBL        ORNE      TASTBL
076600950307     C                   MOVEL     '1'           FINCED
076700001019     C                   ENDIF
076800950307     C*
076900950307     C                   ENDSR
077000950307     C*------------------------------------------------------------------------*
077100990802     C*? CHKTAS - CONTROLLA VALIDITA' DEL RECORD DI TITAS
077200950307     C*------------------------------------------------------------------------*
077300950309     C     CHKTAS        BEGSR
077400950307     C*
077500950307     C                   MOVEL     'S'           WRECOK            1
077600000329     C*
077700001023     C* CONTROLLA LO STATO DELLA CONSEGNA IN RELAZIONE AL TIPO BOLLA
077800001023     C* ==> SE TRATTASI DI BOLLA DI RECUPERO (F3/A3/AP) STORICIZZO COMUNQUE
077900001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
078000001018     C     TASDCM        IFEQ      *ZEROS                                       *NON CONSEGNATA
078100001023     C     TASTBL        ANDNE     'F3'
078200001023     C     TASTBL        ANDNE     'A3'
078300001023     C     TASTBL        ANDNE     'AP'
078400010222     C     TASCBO        ANDNE     'E '
078500010222     C     TASCBO        ANDNE     'B '
078600001019     C                   EXCEPT    ERRDCM
078700000329     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
078800001018     C                   ENDIF
078900001018     C                   ENDIF
079000950309     C*
079100950309     C* CONTROLLA IL CONTRASSEGNO
079200001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
079300950309     C     TASFCA        IFEQ      'S'                                          --- 1 -->
079400950309     C                   Z-ADD     TASAAS        CSBAAS
079500950309     C                   Z-ADD     TASLNP        CSBLNP
079600950309     C                   Z-ADD     TASNRS        CSBNRS
079700950309     C                   Z-ADD     TASNSP        CSBNSP
079800950309     C     KEYCSB        CHAIN     TNCSB03L                           99
079900950309     C     *IN99         IFEQ      '0'                                          --- 2 -->
080000950309     C     CSBSTA        IFNE      9                                            --- 3 -->
080100950309     C     CSBBNA        IFEQ      *ZEROS                                       *NON ANCORA PAGATO
080200950309     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
080300021021     C***                EXCEPT    NOCCSB                                       *STAMPO RECORD
080400021021     C                   ADD       1             CNTCSB                         *CONTEGGIO RECORD
080500950309     C                   ENDIF
080600950309     C                   ENDIF                                                  <-- 3 ---
080700950309     C                   ENDIF                                                  <-- 2 ---
080800950309     C                   ENDIF                                                  <-- 1 ---
080900001018     C                   ENDIF                                                  <-- 1 ---
081000000327     C*
081100000327     C* CONTROLLA I DANNI
081200001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
081300001018     C     TASFDA        IFEQ      'S'
081400040715      **
081500040715      **  Sostituito con il pgm FIDN12R che restituisce l'informazione
081600040715      **   sui danni fra bolla madre e figlia
081700040715      **
081800040715     C**                 Z-ADD     TASAAS        KCTAAS
081900040715     C**                 Z-ADD     TASLNP        KCTLNP
082000040715     C**                 Z-ADD     TASNRS        KCTNRS
082100040715     C**                 Z-ADD     TASNSP        KCTNSP
082200040715     C**   KEYDCT        CHAIN     FNDCT02L                           99
082300040715      *
082400040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
082500040715      * SIA COME MAMMA CHE COME FIGLIA
082600040715     c                   clear                   fidn12ds
082700040715     c                   eval      i12aas = TASAAS
082800040715     c                   eval      i12lnp = TASLNP
082900040715     c                   eval      i12nrs = TASNRS
083000040715     c                   eval      i12nsp = TASNSP
083100040715     c                   eval      i12fel = 'E'
083200040715     c                   call      'FIDN12R'
083300040715     c                   parm                    fidn12ds
083400040715      **
083500040715      * se c'è almeno una C.A. aperta
083600040715      **
083700040715     c     o12nca        ifgt      0
083800040715     C**   *IN99         IFEQ      *OFF                                         *TROVATO DANNO
083900040715      **
084000000327     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
084100021021     C***                EXCEPT    NOCDCT                                       *STAMPO RECORD
084200021021     C                   ADD       1             CNTDCT                         *CONTEGGIO RECORD
084300001018     C                   ENDIF
084400001018     C                   ENDIF
084500001018     C                   ENDIF
084600000404     C*
084700000404     C* CONTROLLA LE GIACENZE
084800001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
084900001018     C     TASFGC        IFEQ      'S'
085000000404     C                   Z-ADD     TASAAS        KCPAAS
085100000404     C                   Z-ADD     TASLNP        KCPLNP
085200000404     C                   Z-ADD     TASNRS        KCPNRS
085300000404     C                   Z-ADD     TASNSP        KCPNSP
085400000404     C                   Z-ADD     0             KCPFRG                         *PRIMO PRG
085500050301     C     KEYGCP        CHAIN(N)  TIGCP51L                           99
085600001018     C     *IN99         IFEQ      *OFF
085700001018     C     GCPFAS        IFLT      50                                           *FASI APERTE
085800000404     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
085900021021     C***                EXCEPT    NOCGCP                                       *STAMPO RECORD
086000021021     C                   ADD       1             CNTGCP                         *CONTEGGIO RECORD
086100001018     C                   ENDIF
086200001018     C                   ENDIF
086300001018     C                   ENDIF
086400001018     C                   ENDIF
086500011227     C*
086600020220     C* SE BOLLA POSTA CANCELLARE SOLO SE + VECCHIA D 2 ANNI
086700011227     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
086800011227     C     TASTSP        IFEQ      'P'                                          *BOLLA POSTA
086900020220     C     TASDSP        ANDGT     DSDATFP
087000011227     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
087100011227     C                   ENDIF
087200011227     C                   ENDIF
087300001019     C*
087400001019     C* CONTEGGIO BOLLE SCARTATE
087500001019     C     WRECOK        IFEQ      'N'
087600001019     C                   ADD       1             NUMNON
087700001019     C                   ENDIF
087800950307     C*
087900950307     C                   ENDSR
088000950309     C*------------------------------------------------------------------------*
088100950309     C*?AGGTAB - AGGIORNA LA TABELLA '$3' CON ULTIMA DATA TRASF. TAS00 > TAS10
088200950309     C*------------------------------------------------------------------------*
088300950309     C     AGGTAB        BEGSR
088400950309     C*
088500950309     C                   OPEN      TABEL00F
088600950309     C*
088700950309     C                   Z-ADD     1             KTBUTE
088800950309     C                   MOVEL     '$3'          KTBCOD
088900950309     C                   MOVEL     '1'           KTBKEY
089000950309     C     KEYTAB        CHAIN     TABEL00F                           99
089100950309     C     *IN99         IFEQ      *ON                                          *NON TROVATA
089200950309     C                   ELSE                                                   *TROVATA
089300950309     C                   MOVEL     TBLUNI        DS$3
089400950309     C                   Z-ADD     PARDTF        §$3DP0                         *ULT.DATA TRASF P>SA
089500020220     C                   Z-ADD     PARDFP        §$3DPP                         *ULT.DATA TRASF P>SA
089600950309     C                   MOVEL     DS$3          TBLUNI
089700950309     C                   UPDATE    TABEL
089800950309     C                   ENDIF
089900950309     C*
090000950309     C                   CLOSE     TABEL00F
090100950309     C*
090200950309     C                   ENDSR
090300950717     C*------------------------------------------------------------------------*
090400950717     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
090500950717     C*------------------------------------------------------------------------*
090600950717     C     SUBDAT        BEGSR
090700950717     C                   TIME                    DTTIME
090800950717     C                   Z-ADD     DTANN         DSANN
090900950717     C                   Z-ADD     DTMES         DSMES
091000950717     C                   Z-ADD     DTGIO         DSGIO
091100950717     C                   Z-ADD     DTSEC         DSSEC
091200950717     C                   ENDSR
091300940912     C*------------------------------------------------------------------------*
091400050705     C*? funzione per scivere file x Laguna
091500940912     C*------------------------------------------------------------------------*
091600050705     C     srlaguna      BEGSR
091700050705     c                   clear                   tnsb34ds
091800050929     c* ricavo il progressivo da impostare nella directory ogni 450000 bolle
091900050720     c* cancellate cambio directory
092000050720     C                   ADD       1             NUMlag                         *REC TRASF TITAS
092100050929     c     numlag        div       450000        sb34prg
092200050720     c                   add       1             sb34prg
092300050705     c                   eval      sb34tla = 'R'
092400050705     c                   eval      sb34aas = tasaas
092500050705     c                   eval      sb34lnp = taslnp
092600050705     c                   eval      sb34nrs = tasnrs
092700050705     c                   eval      sb34nsp = tasnsp
092800050706     c                   eval      sb34dtp = pardtf
092900050706     c                   eval      sb34cop = parcop
093000050705     c                   call      'TNSB34R'
093100050705     c                   parm                    tnsb34ds
093200050705     c                   if        sb34err <> ' '
093300050705     c                   end
093400050705     C                   ENDSR
093401061122     C*------------------------------------------------------------------------*
093402061122     C*? WRTTASSTO - SCRITTURA FILE STORICO TITASS
093403061122     C*------------------------------------------------------------------------*
093404061122     C     WRTTASSTO     BEGSR
093405080312     c* memorizzo directory laguna per sapere dove è stata archiviata la spe
093406080312     c                   clear                   tasdtfp
093407080312     c                   if        §floiml <> ' '
093408080312     c                   move      tasaas        ladaas
093409080312     c                   move      taslnp        ladlnp
093410080312     c                   move      tasnrs        ladnrs
093411080312     c                   move      tasnsp        ladnsp
093412080312     c     klad          chain     tilad01l
093413080312     c                   If        %found(tilad01l)
093414080312     c                   movel     laddir        tasdtfp
093415080312     c                   end
093416080312     c                   end
093417061122      * Decodifica padroncino
093418061122     c                   If        taspdc <> *zeros
093419061122     c     Keyapd        chain     fiapd01l
093420061122     c                   If        %found(fiapd01l)
093421061122     c                   eval      taspdd = apdrsc
093422061122     c                   endif
093423061122     c                   endif
093424061122
093425061122      * recupero c/assegno
093426061122     c                   if        tasfca = 'S'
093427061122     c                   z-add     csbcas        Tascas
093429061122     c                   movel     csbvca        tasvca
093433061122     c                   endif
093434061122
093435061122     c                   move      tasksc        com04
093436061122      * PARTITA IVA / CODICE FISCALE
093437061122     c                   If        %subst(tastbl:1:1) = 'A'  and
093438061122     c                             tascpd <> *blanks
093439061122     c                   eval      tasiva  = tascpd
093440061122     c                   endif
093441061122      * se partita iva non valorizzata
093442061122     c                   If        tasiva = *blanks  or tascdf = *blanks
093443061122      * codificato franco o assegnato
093444061122     c                   if        com04  <>  9999  and com04  <>  8888
093445061122     c                   exsr      reciva
093446061122     c                   if        o69err <> *on
093447061122     c                   if        tascdf = *blanks
093448061122     c                   eval      tasiva = indiva
093449061122     c                   endif
093450061122      * se devo cercare anche il codife fiscale
093451061122     c                   if        tascdf = *blanks
093452061122     c                   eval      tascdf = indcdf
093453061122     c                   endif
093454061122
093455061122     c                   endif
093456061122     c                   endif
093457061122     c                   endif
093458061124      * valorizzo il flag che mi identifica le spedizioni senza segnacollo
093459061124     c                   If        Tasfns <> ' S' and tasncd = *zeros
093460061124     c                   eval      tassss = 'S'
093461061124     c                   endif
093462061122
093463061122     c                   write     titassto                             99
093464061122     C   99              EXCEPT    ERRsto
093465061122     C   99              MOVEL     '1'           FINL00                         *FINE PGM
093466061122     C  N99              ADD       1             NUMsto                         *REC TRASF FNEVB
093467061122
093468061122     C                   ENDSR
093469061122     C*
093500050705     C*------------------------------------------------------------------------*
093600050705     C*? RUTINZ - ROUTINE INIZIALE
093700050705     C*------------------------------------------------------------------------*
093800050705     C     RUTINZ        BEGSR
093900931115     C*
094000931115     C* PARAMETRI IN INPUT AL PGM
094100931115     C     *ENTRY        PLIST
094200931115     C                   PARM                    KPJBA
094300931115     C                   MOVEL     KPJBU         PARAM
094400950309     C*
094500950309     C* DEFINISCO CAMPI
094600950309     C     *LIKE         DEFINE    TBLKUT        KTBUTE
094700950309     C     *LIKE         DEFINE    TBLCOD        KTBCOD
094800950309     C     *LIKE         DEFINE    TBLKEY        KTBKEY
094900970903     C     *LIKE         DEFINE    CETNBC        MINNBC
095000040715     C***  *LIKE         DEFINE    DCTAAS        KCTAAS                         *FNDCT00F
095100040715     C***  *LIKE         DEFINE    DCTLNP        KCTLNP
095200040715     C***  *LIKE         DEFINE    DCTNRS        KCTNRS
095300040715     C***  *LIKE         DEFINE    DCTNSP        KCTNSP
095400050301     C     *LIKE         DEFINE    GCPAAS        KCPAAS                         *TIGCP00F
095500000404     C     *LIKE         DEFINE    GCPLNP        KCPLNP
095600000404     C     *LIKE         DEFINE    GCPNRS        KCPNRS
095700000404     C     *LIKE         DEFINE    GCPNSP        KCPNSP
095800000404     C     *LIKE         DEFINE    GCPFRG        KCPFRG
095900050301     C     *LIKE         DEFINE    GAPAAS        KAPAAS                         *TIGAP00F
096000000404     C     *LIKE         DEFINE    GAPLNP        KAPLNP
096100000404     C     *LIKE         DEFINE    GAPNRS        KAPNRS
096200000404     C     *LIKE         DEFINE    GAPNSP        KAPNSP
096300050301     C     *LIKE         DEFINE    GNPAGC        KNPAGC                         *TIGNP00F
096400000404     C     *LIKE         DEFINE    GNPFGC        KNPFGC
096500000404     C     *LIKE         DEFINE    GNPNGC        KNPNGC
096600950309     C*
096700950309     C* CHIAVE LETTURA TABEL00F - COMPLETA
096800950309     C     KEYTAB        KLIST
096900950309     C                   KFLD                    KTBUTE
097000950309     C                   KFLD                    KTBCOD
097100950309     C                   KFLD                    KTBKEY
097200950309     C*
097300950309     C* CHIAVE LETTURA TNCSB01L - COMPLETA
097400950309     C     KEYCSB        KLIST
097500950309     C                   KFLD                    CSBAAS
097600950309     C                   KFLD                    CSBLNP
097700950309     C                   KFLD                    CSBNRS
097800950309     C                   KFLD                    CSBNSP
097900000327     C*
098000000327     C* CHIAVE LETTURA FNDCT02L - COMPLETA
098100040715     C***  KEYDCT        KLIST
098200040715     C***                KFLD                    KCTAAS
098300040715     C***                KFLD                    KCTLNP
098400040715     C***                KFLD                    KCTNRS
098500040715     C***                KFLD                    KCTNSP
098600930208     C*
098700990802     C* CHIAVE LETTURA TITAS38C - PARZIALE
098800930621     C     KEYTAS        KLIST
098900941123     C                   KFLD                    TASAAS
099000941123     C                   KFLD                    TASMGS
099100950307     C*
099200990802     C* CHIAVE LETTURA TITA730C - COMPLETA
099300950307     C     KEYTA7        KLIST
099400950307     C                   KFLD                    TA7AAS
099500950307     C                   KFLD                    TA7LNP
099600950307     C                   KFLD                    TA7NRS
099700950307     C                   KFLD                    TA7NSP
099800950307     C                   KFLD                    TA7TBL
099900990802     C*
100000990802     C* CHIAVE LETTURA TITAA30C - COMPLETA
100100990802     C     KEYTAA        KLIST
100200990802     C                   KFLD                    TAAAAS
100300990802     C                   KFLD                    TAALNP
100400990802     C                   KFLD                    TAANRS
100500990802     C                   KFLD                    TAANSP
100600990802     C*
100700990802     C* CHIAVE LETTURA TITAT30C - COMPLETA
100800990802     C     KEYTAT        KLIST
100900990802     C                   KFLD                    TATAAS
101000990802     C                   KFLD                    TATLNP
101100990802     C                   KFLD                    TATNRS
101200990802     C                   KFLD                    TATNSP
101300990802     C*
101400990802     C* CHIAVE LETTURA TITA430C - COMPLETA
101500990802     C     KEYTA4        KLIST
101600990802     C                   KFLD                    TA4AAS
101700990802     C                   KFLD                    TA4LNP
101800990802     C                   KFLD                    TA4NRS
101900990802     C                   KFLD                    TA4NSP
102000010903     C*
102100010903     C* CHIAVE LETTURA TITAH30C - COMPLETA
102200010903     C     KEYTAH        KLIST
102300010903     C                   KFLD                    TAHAAS
102400010903     C                   KFLD                    TAHLNP
102500010903     C                   KFLD                    TAHNRS
102600010903     C                   KFLD                    TAHNSP
102700950209     C*
102800950307     C* CHIAVE LETTURA ECCET30C - COMPLETA
102900950220     C     KEYCET        KLIST
103000950220     C                   KFLD                    CETAAS                         ANNO SPEDIZIONE
103100950220     C                   KFLD                    CETLNP                         LINEA DI PARTENZA
103200950220     C                   KFLD                    CETNRS                         NUMERO SERIE
103300950220     C                   KFLD                    CETNSP                         NUMERO SPEDIZIONE
103400950220     C                   KFLD                    CETTBL                         TIPO BOLLA
103500950307     C*
103600950307     C* CHIAVE LETTURA ECCED30C - COMPLETA
103700950307     C     KEYCED        KLIST
103800950307     C                   KFLD                    CEDAAS                         ANNO SPEDIZIONE
103900950307     C                   KFLD                    CEDLNP                         LINEA DI PARTENZA
104000950307     C                   KFLD                    CEDNRS                         NUMERO SERIE
104100950307     C                   KFLD                    CEDNSP                         NUMERO SPEDIZIONE
104200950307     C                   KFLD                    CEDTBL                         TIPO BOLLA
104300980325     C*
104400980325     C* CHIAVE LETTURA ECFIM30C - COMPLETA
104500980325     C     KEYFIM        KLIST
104600980325     C                   KFLD                    FIMAAS                         ANNO SPEDIZIONE
104700980325     C                   KFLD                    FIMLNP                         LINEA DI PARTENZA
104800980325     C                   KFLD                    FIMNRS                         NUMERO SERIE
104900980325     C                   KFLD                    FIMNSP                         NUMERO SPEDIZIONE
105000980325     C                   KFLD                    FIMTBL                         TIPO BOLLA
105100000327     C*
105200000327     C* CHIAVE LETTURA FNEVB01L - COMPLETA
105300000327     C     KEYEVB        KLIST
105400000327     C                   KFLD                    EVBAAS                         ANNO SPEDIZIONE
105500000327     C                   KFLD                    EVBLNP                         LINEA DI PARTENZA
105600000327     C                   KFLD                    EVBNRS                         NUMERO SERIE
105700000327     C                   KFLD                    EVBNSP                         NUMERO SPEDIZIONE
105800000404     C*
105900050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
106000000404     C     KEYGCP        KLIST
106100000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
106200000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
106300000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
106400000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
106500000404     C                   KFLD                    KCPFRG                         PRG RIAPERTURE
106600000404     C*
106700050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
106800000404     C     KE2GCP        KLIST
106900000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
107000000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
107100000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
107200000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
107300000404     C*
107400050301     C* CHIAVE LETTURA TIGAP01L - PARZIALE
107500000404     C     KEYGAP        KLIST
107600000404     C                   KFLD                    KAPAAS                         ANNO SPEDIZIONE
107700000404     C                   KFLD                    KAPLNP                         LINEA DI PARTENZA
107800000404     C                   KFLD                    KAPNRS                         NUMERO SERIE
107900000404     C                   KFLD                    KAPNSP                         NUMERO SPEDIZIONE
108000000404     C*
108100050301     C* CHIAVE LETTURA TIGNP01L - PARZIALE
108200000404     C     KEYGNP        KLIST
108300000404     C                   KFLD                    KNPAGC                         ANNO APERTURA
108400000404     C                   KFLD                    KNPFGC                         FILILAE APERTURA
108500000404     C                   KFLD                    KNPNGC                         NUMERO GIACENZA
108600040303     C*
108700040531     C* CHIAVE LETTURA FIAR531C
108800040303     C     KEYAR5        KLIST
108900040303     C                   KFLD                    AR5AAS
109000040303     C                   KFLD                    AR5LNP
109100040303     C                   KFLD                    AR5NRS
109200040303     C                   KFLD                    AR5NSP
109201060919     C*
109202060919     C* CHIAVE LETTURA TITAI01L
109203060919     C     KEYtai        KLIST
109204060919     C                   KFLD                    taiAAS
109205060919     C                   KFLD                    taiLNP
109206060919     C                   KFLD                    taiNRS
109207060919     C                   KFLD                    taiNSP
109208060919     C                   KFLD                    taiTBL
109209061122
109210061122     C* CHIAVE LETTURA Fiapd01l
109211061122     C     KEYapd        KLIST
109212061122     C                   KFLD                    trcapd
109213061122     C                   KFLD                    taspdc
109300040823      *
109400040823     C     kbol          klist
109500040823     C                   kfld                    tasaas
109600040823     C                   kfld                    taslnp
109700040823     C                   kfld                    tasnrs
109800040823     C                   kfld                    tasnsp
109801080312      *
109802080312     C     klad          klist
109803080312     C                   kfld                    LADAAS
109804080312     C                   kfld                    LADLNP
109805080312     C                   kfld                    LADNRS
109806080312     C                   kfld                    LADNSP
109900000327     C*
110000000327     C* CALCOLA LA DATA CORRENTE
110100000327     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
110200000327     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
110300000327     C                   Z-ADD     N8            G08DAT
110400000327     C                   Z-ADD     *ZEROS        G08INV
110500000327     C                   MOVEL     '0'           G08ERR
110600000327     C                   CALL      'XSRDA8'
110700000327     C                   PARM                    WLBDA8
110800000327     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
110900930208     C*
111000930208     C* IMPOSTO A ZERO IL FINE PGM
111100930208     C                   MOVEL     '0'           FINL00            1
111200931105     C                   MOVEL     *BLANKS       RSUT             20
111300040922     C                   Z-ADD     *ZEROS        NUMblp            9 0
111400040922     C                   Z-ADD     *ZEROS        NUMarb            9 0
111500950308     C                   Z-ADD     *ZEROS        NUMTAS            9 0
111600050720     C                   Z-ADD     *ZEROS        NUMlag            9 0
111700950308     C                   Z-ADD     *ZEROS        NUMTA7            9 0
111800990802     C                   Z-ADD     *ZEROS        NUMTAA            9 0
111900990802     C                   Z-ADD     *ZEROS        NUMTAT            9 0
112000990802     C                   Z-ADD     *ZEROS        NUMTA4            9 0
112100010903     C                   Z-ADD     *ZEROS        NUMTAH            9 0
112200950307     C                   Z-ADD     *ZEROS        NUMCET            9 0
112300950308     C                   Z-ADD     *ZEROS        NUMCED            9 0
112400980325     C                   Z-ADD     *ZEROS        NUMFIM            9 0
112500970904     C                   Z-ADD     *ZEROS        NUMECT            9 0
112600970904     C                   Z-ADD     *ZEROS        NUMECD            9 0
112700000327     C                   Z-ADD     *ZEROS        NUMEVB            9 0
112800000404     C                   Z-ADD     *ZEROS        NUMGCP            9 0
112900000404     C                   Z-ADD     *ZEROS        NUMGAP            9 0
113000000404     C                   Z-ADD     *ZEROS        NUMGNP            9 0
113100001019     C                   Z-ADD     *ZEROS        NUMNON            9 0
113200040303     C                   Z-ADD     *ZEROS        NUMAR5            9 0
113201060919     C                   Z-ADD     *ZEROS        NUMtai            9 0
113202061122     C                   Z-ADD     *ZEROS        NUMsto            9 0
113300021021     C                   Z-ADD     *ZEROS        CNTGCP            9 0
113400021021     C                   Z-ADD     *ZEROS        CNTDCT            9 0
113500021021     C                   Z-ADD     *ZEROS        CNTCSB            9 0
113600040823      * Apro il Fnblp di filiale
113700040823     c                   Clear                   comman
113800040823     c                   Movel(p)  cmd(1)        comman
113900040823     c                   Eval      %Subst(comman:13:8) = 'FNBLP01L'
114000040910      *test sistema informativo di prova
114100040910     c                   if        %subst(knsif:7:1) = 'P'
114200040910     c                   Eval      %Subst(comman:30:10) = wlibp
114300040910     c                   else
114400040910     c                   Eval      %Subst(comman:30:10) = wlib
114500040910     c                   endif
114600040910
114700040823     c                   Eval      comman =
114800040823     c                             %trim(comman) + '/FNBLP01L)'
114900040823     c                   Call      'QCMDEXC'                            99
115000040823     c                   Parm                    comman
115100040823     c                   Parm                    lenght
115200040823     c                   If        *In99
115300040823     c                   seton                                        lr
115400040823     c                   Else
115500040823     c                   Open      Fnblp01l
115600040823     c                   EndIf
115700040823      *
115800040823      * Apro  Fnarb di filiale
115900040823     c                   Clear                   comman
116000040823     c                   Movel(p)  cmd(1)        comman
116100040823     c                   Eval      %Subst(comman:13:8) = 'FNARB01L'
116200040910      *test sistema informativo di prova
116300040910     c                   if        %subst(knsif:7:1) = 'P'
116400040910     c                   Eval      %Subst(comman:30:10) = wlibp
116500040910     c                   else
116600040910     c                   Eval      %Subst(comman:30:10) = wlib
116700040910     c                   endif
116800040910
116900040823     c                   Eval      comman =
117000040823     c                             %trim(comman) + '/FNARB01L)'
117100040823     c                   Call      'QCMDEXC'                            99
117200040823     c                   Parm                    comman
117300040823     c                   Parm                    lenght
117400040823     c                   If        *In99
117500040823     c                   seton                                        lr
117600040823     c                   Else
117700040823     c                   Open      Fnarb01l
117800040823     c                   EndIf
117900040823      *
118000931115     C*
118100950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
118200000000     C*
118300000000     C                   ENDSR
118400940912     O*------------------------------------------------------------------------*
118500940905     OPRTF198   E            TESTA            02
118600950309     O                                            8 'TNSB38R '
118700931105     O                       RSUT                45
118800990802     O                                           82 'TRASF. TITASP -> SALVAT.'
118900931115     O                                           90 'DAL:'
119000950309     O                       PARDTI              99
119100931115     O                                          110 'AL:'
119200950309     O                       PARDTF             119
119300020220     O                                          128 '/PT'
119400020220     O                       PARDFP             132
119500931105     O                                          150 'Data:'
119600950717     O                       DSDAT6             159 '  /  /  '
119700931105     O                                          167 'Ora:'
119800950717     O                       DTORA6             178 '  .  .  '
119900931105     O                                          190 'Pagina:'
120000931105     O                       PAGE          Z    198
120100931105     O          E            TESTA       1  1
120200931105     O                       TES(2)              66
120300931105     O                       TES(3)             132
120400931105     O                       TES(4)             198
120500931105     O          E            TESTA          1
120600931105     O                       TES(1)              66
120700931105     O                       TES(1)             132
120800931105     O                       TES(1)             198
120900001019     O*------------------------------------
121000001019     O          E            ERRDCM         1
121100001019     O                       TASAAS            +  5
121200001019     O                       TASLNP            +  1
121300001019     O                       TASNRS            +  1
121400001019     O                       TASNSP            +  1
121500001019     O                       TASTBL            +  1
121600001019     O                                         +  1 'BOLLA NON CONS.    '
121700931105     O*------------------------------------
121800950307     O          E            ERRTAS         1
121900950307     O                       TASAAS            +  5
122000950307     O                       TASLNP            +  1
122100950307     O                       TASNRS            +  1
122200950307     O                       TASNSP            +  1
122300950307     O                       TASTBL            +  1
122400990802     O                                         +  1 'ERR. WRITE TITAS   '
122500950307     O*------------------------------------
122600950307     O          E            ERRTA7         1
122700950307     O                       TA7AAS            +  5
122800950307     O                       TA7LNP            +  1
122900950307     O                       TA7NRS            +  1
123000950307     O                       TA7NSP            +  1
123100950307     O                       TA7TBL            +  1
123200950307     O                       TA7SVN            +  1
123300990802     O                                         +  1 'ERR. WRITE TITA7   '
123400990802     O*------------------------------------
123500990802     O          E            ERRTAA         1
123600990802     O                       TAAAAS            +  5
123700990802     O                       TAALNP            +  1
123800990802     O                       TAANRS            +  1
123900990802     O                       TAANSP            +  1
124000990802     O                       TAATRC            +  1
124100990802     O                                         +  1 'ERR. WRITE TITAA   '
124200990802     O*------------------------------------
124300990802     O          E            ERRTAT         1
124400990802     O                       TATAAS            +  5
124500990802     O                       TATLNP            +  1
124600990802     O                       TATNRS            +  1
124700990802     O                       TATNSP            +  1
124800990802     O                       TATNSC            +  1
124900990802     O                                         +  1 'ERR. WRITE TITAT   '
125000990802     O*------------------------------------
125100990802     O          E            ERRTA4         1
125200990802     O                       TA4AAS            +  5
125300990802     O                       TA4LNP            +  1
125400990802     O                       TA4NRS            +  1
125500990802     O                       TA4NSP            +  1
125600990802     O                       TA4TRC            +  1
125700990802     O                                         +  1 'ERR. WRITE TITA4   '
125800010903     O*------------------------------------
125900010903     O          E            ERRTAH         1
126000010903     O                       TAHAAS            +  5
126100010903     O                       TAHLNP            +  1
126200010903     O                       TAHNRS            +  1
126300010903     O                       TAHNSP            +  1
126400010903     O                       TAHTRC            +  1
126500010903     O                                         +  1 'ERR. WRITE TITAH   '
126600950307     O*------------------------------------
126700950307     O          E            ERRCET         1
126800950307     O                       CETAAS            +  5
126900950307     O                       CETLNP            +  1
127000950307     O                       CETNRS            +  1
127100950307     O                       CETNSP            +  1
127200950307     O                       CETTBL            +  1
127300950309     O                                         +  1 'ERR. WRITE ECCET   '
127400950307     O*------------------------------------
127500950307     O          E            ERRCED         1
127600950307     O                       CEDAAS            +  5
127700950307     O                       CEDLNP            +  1
127800950307     O                       CEDNRS            +  1
127900950307     O                       CEDNSP            +  1
128000950307     O                       CEDTBL            +  1
128100950307     O                       CEDCMP            +  1
128200950309     O                                         +  1 'ERR. WRITE ECCED   '
128300980325     O*------------------------------------
128400980325     O          E            ERRFIM         1
128500980325     O                       FIMAAS            +  5
128600980325     O                       FIMLNP            +  1
128700980325     O                       FIMNRS            +  1
128800980325     O                       FIMNSP            +  1
128900980325     O                       FIMTBL            +  1
129000980325     O                                         +  1 'ERR. WRITE ECFIM   '
129100970903     O*------------------------------------
129200970903     O          E            ERRECT         1
129300970903     O                       ECTNBC        Z   +  5
129400970903     O                       ECTCMP        Z   +  1
129500970903     O                       ECTDDC            +  1 '    /  /  '
129600970903     O                                         +  1 'ERR. WRITE ECECT   '
129700970903     O*------------------------------------
129800970903     O          E            ERRECD         1
129900970903     O                       ECDNBC        Z   +  5
130000970903     O                       ECDCMP        Z   +  1
130100970903     O                       ECDFRA            +  1
130200970903     O                       ECDIOE            +  1
130300970903     O                       ECDKCC        Z   +  1
130400970903     O                       ECDKSC        Z   +  1
130500970903     O                       ECDRIC            +  1
130600970903     O                       ECDCCF        Z   +  1
130700970903     O                       ECDDDO            +  1 '    /  /  '
130800970903     O                                         +  1 'ERR. WRITE ECECD   '
130900000404     O*------------------------------------
131000000404     O          E            ERREVB         1
131100000404     O                       EVBAAS            +  5
131200000404     O                       EVBLNP            +  1
131300000404     O                       EVBNRS            +  1
131400000404     O                       EVBNSP            +  1
131500000404     O                                         +  1 'ERR. WRITE FNEVB   '
131600000404     O*------------------------------------
131700000404     O          E            ERRGCP         1
131800000404     O                       GCPAAS            +  5
131900000404     O                       GCPLNP            +  1
132000000404     O                       GCPNRS            +  1
132100000404     O                       GCPNSP            +  1
132200050301     O                                         +  1 'ERR. WRITE TIGCP   '
132300050303     O          E            ERRGCP1        1
132400050303     O                       GCPAAS            +  5
132500050303     O                       GCPLNP            +  1
132600050303     O                       GCPNRS            +  1
132700050303     O                       GCPNSP            +  1
132800050303     O                                         +  1 'GIACENZA ANCORA IN FILIALE'
132900000404     O*------------------------------------
133000000404     O          E            ERRGAP         1
133100000404     O                       GAPAAS            +  5
133200000404     O                       GAPLNP            +  1
133300000404     O                       GAPNRS            +  1
133400000404     O                       GAPNSP            +  1
133500050301     O                                         +  1 'ERR. WRITE TIGAP   '
133600000404     O*------------------------------------
133700000404     O          E            ERRGNP         1
133800000404     O                       GNPAGC            +  5
133900000404     O                       GNPFGC            +  4
134000000404     O                       GNPNGC            +  1
134100050301     O                                         +  1 'ERR. WRITE TIGNP   '
134200040303     O*------------------------------------
134300040303     O          E            ERRAR5         1
134400040303     O                       AR5AAS            +  5
134500040303     O                       AR5LNP            +  1
134600040303     O                       AR5NRS            +  1
134700040303     O                       AR5NSP            +  1
134800040303     O                                         +  1 'ERR. WRITE FIAR5   '
134801060919     O          E            ERRtai         1
134802060919     O                       taiAAS            +  5
134803060919     O                       taiLNP            +  1
134804060919     O                       taiNRS            +  1
134805060919     O                       taiNSP            +  1
134806060919     O                       taitbl            +  1
134807060919     O                                         +  1 'ERR. WRITE TITAI   '
134900950309     O*------------------------------------
135000000404     O          E            NOCCSB         1
135100950309     O                       CSBAAS            +  5
135200950309     O                       CSBLNP            +  1
135300950309     O                       CSBNRS            +  1
135400950309     O                       CSBNSP            +  1
135500000404     O                                         +  1 'BOLLA NON CANC:TNCSB'
135600000404     O*------------------------------------
135700040715     O*****     E            NOCDCT         1
135800040715     O*****                  DCTAAS            +  5
135900040715     O*****                  DCTLNP            +  1
136000040715     O*****                  DCTNRS            +  1
136100040715     O*****                  DCTNSP            +  1
136200040715     O*****                                    +  1 'BOLLA NON CANC:FNDCT'
136300040715     O          E            NOCDCT         1
136400040715     O                       i12AAS            +  5
136500040715     O                       i12LNP            +  1
136600040715     O                       i12NRS            +  1
136700040715     O                       i12NSP            +  1
136800040715     O                                         +  1 'BOLLA NON CANC:FNDCT'
136900000404     O*------------------------------------
137000000404     O          E            NOCGCP         1
137100000404     O                       GCPAAS            +  5
137200000404     O                       GCPLNP            +  1
137300000404     O                       GCPNRS            +  1
137400000404     O                       GCPNSP            +  1
137500050301     O                                         +  1 'BOLLA NON CANC:TIGCP'
137600040922     O*------------------------------------
137700040922     O          E            ERRblp         1
137800040922     O                       blpAAS            +  5
137900040922     O                       blpLNP            +  1
138000040922     O                       blpNRS            +  1
138100040922     O                       blpNSP            +  1
138200040922     O                                         +  1 'ERR. Trovato reK su FNBLP'
138300040922     O*------------------------------------
138400040922     O          E            ERRarb         1
138500040922     O                       arbAAS            +  5
138600040922     O                       arbLNP            +  1
138700040922     O                       arbNRS            +  1
138800040922     O                       arbNSP            +  1
138900040922     O                                         +  1 'ERR. Trovato reK su FNARB'
138906061122     O          E            ERRSTO         1
138907061122     O                       TASAAS            +  5
138908061122     O                       TASLNP            +  1
138909061122     O                       TASNRS            +  1
138910061122     O                       TASNSP            +  1
138911061122     O                       TASTBL            +  1
138912061122     O                                         +  1 'ERR. WRITE STORICO '
139000040922     O*------------------------------------
139100950308     O          E            STAFIN         1
139200990802     O                                           30 'N.REC.TRASF. TITAS'
139300950308     O                       NUMTAS        2     44
139400950308     O          E            STAFIN         1
139500990802     O                                           30 'N.REC.TRASF. TITA7'
139600950308     O                       NUMTA7        2     44
139700990802     O          E            STAFIN         1
139800990802     O                                           30 'N.REC.TRASF. TITAA'
139900990802     O                       NUMTAA        2     44
140000990802     O          E            STAFIN         1
140100990802     O                                           30 'N.REC.TRASF. TITAT'
140200990802     O                       NUMTAT        2     44
140300990802     O          E            STAFIN         1
140400990802     O                                           30 'N.REC.TRASF. TITA4'
140500990802     O                       NUMTA4        2     44
140600010903     O          E            STAFIN         1
140700010903     O                                           30 'N.REC.TRASF. TITAH'
140800010903     O                       NUMTAH        2     44
140900950308     O          E            STAFIN         1
141000950308     O                                           30 'N.REC.TRASF. ECCET'
141100950308     O                       NUMCET        2     44
141200950308     O          E            STAFIN         1
141300950308     O                                           30 'N.REC.TRASF. ECCED'
141400950308     O                       NUMCED        2     44
141500980325     O          E            STAFIN         1
141600980325     O                                           30 'N.REC.TRASF. ECFIM'
141700980325     O                       NUMFIM        2     44
141800970903     O          E            STAFIN         1
141900970903     O                                           30 'N.REC.TRASF. ECECT'
142000970903     O                       NUMECT        2     44
142100970903     O          E            STAFIN         1
142200970903     O                                           30 'N.REC.TRASF. ECECD'
142300970903     O                       NUMECD        2     44
142400000327     O          E            STAFIN         1
142500000327     O                                           30 'N.REC.TRASF. FNEVB'
142600000327     O                       NUMEVB        2     44
142700000404     O          E            STAFIN         1
142800050301     O                                           30 'N.REC.TRASF. TIGCP'
142900000404     O                       NUMGCP        2     44
143000000404     O          E            STAFIN         1
143100050301     O                                           30 'N.REC.TRASF. TIGAP'
143200000404     O                       NUMGAP        2     44
143300000404     O          E            STAFIN         1
143400050301     O                                           30 'N.REC.TRASF. TIGNP'
143500000404     O                       NUMGNP        2     44
143600040310     O          E            STAFIN         1
143700040310     O                                           30 'N.REC. BOLLE FIAR5'
143800040310     O                       NUMAR5        2     44
143801060919     O          E            STAFIN         1
143802060919     O                                           30 'N.REC. BOLLE TITAI'
143803060919     O                       NUMTAI        2     44
143804061122     O          E            STAFIN         1
143805061122     O                                           30 'N.BOLLE IN STORICO'
143806061122     O                       NUMSTO        2     44
143900001019     O          E            STAFIN         1
144000001020     O                                           30 'N.REC. BOLLE SCART'
144100001019     O                       NUMNON        2     44
144200021021     O          E            STAFIN         1
144300021021     O                                           30 'DI CUI:'
144400021021     O          E            STAFIN         1
144500021021     O                                           30 'X GIACENZA APERTA '
144600021021     O                       CNTGCP        2     44
144700021021     O          E            STAFIN         1
144800021021     O                                           30 'X DANNO PENDENTE  '
144900021021     O                       CNTDCT        2     44
145000021021     O          E            STAFIN         1
145100021021     O                                           30 'X C/A ATTIVO      '
145200021021     O                       CNTCSB        2     44
145300040922     O          E            STAFIN         1
145400040922     O                                           30 'N.REK.Trovati FNBLP'
145500040922     O                       NUMblp        2     44
145600040922     O          E            STAFIN         1
145700040922     O                                           30 'N.REK.Trovati FNARB'
145800040922     O                       NUMarb        2     44
145801060626     O          E            STAFIN         1
145802060626     O                                           +0 'Chiusuara pgm: FINTAS'
145803060626     O                       fintas              +1
145804060626     O                       tasdsp              +1
145805060626     O                       dsdatf              +1
145806060626     O               98                          +1 'SHTDN'
145807060626     O                                           +1 'FINL00'
145808060626     O                       finl00              +1
145900950308     O          E            STAFIN           60
146000931115     O                                           76 '*** FINE STAMPA ***'
146100931115     O                                          150 'Data:'
146200950717     O                       DSDAT6             159 '  /  /  '
146300990802     O                                          167 'Ora:'
146400950717     O                       DTORA6             178 '  :  :  '
146500940912     O*------------------------------------------------------------------------*
146600970903     O* AGGIORNAMENTO FILES
146700970903     O*------------------------------------------------------------------------*
146800970903     OECECT     E            AGGECT
146900970903     O                       ECTATB
147000970903     O                       ECTFTR
147100970903     OECECD     E            AGGECD
147200970903     O                       ECDATB
147300970903     O                       ECDFTR
147400970903     O*------------------------------------------------------------------------*
147500040823** cmd
147600040823OVRDBF FILE(        ) TOFILE(
147700931105**  TES - RIGHE IN STAMPA
147800931105------------------------------------------------------------------  1
147900931105 DATA    ORA       AA LNP SE N.SPED. TB     ERR  DESCRIZIONE ERROR  2
148000950308E                                                                   3
148100931105                                                                    4
148200931105                                                                    5
148300931105                                                                    6
148400931105                                                                    7
148500931105                                                                    8
148600931105                                                                    9
148700931105                                                                   10
148800931105                                                                   11
148900931105                                                                   12
149000931105                                                                   13
149100931105                                                                   14
149200931105                                                                   15
