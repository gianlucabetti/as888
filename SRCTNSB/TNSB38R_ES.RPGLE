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
001300061121     FTITASS0F  O  A E           K DISK
001400061121     F                                     RENAME(TITASS00:TITASSTO)
001500990802     F*---
001600990802     F* ESTENSIONE BOLLE
001700990802     F*---
001800990802     FTITA730C  UF   E           K DISK
001900990802     FTITA7P0F  O  A E           K DISK
002000990802     F                                     RENAME(TITA7P00:TITA7S00)
002100990802     F*---
002200990802     F* ANAGRAFICHE
002300990802     F*---
002400990802     FTITAA30C  UF   E           K DISK
002500990802     FTITAAP0F  O  A E           K DISK
002600990802     F                                     RENAME(TITAAP00:TITAAS00)
002700990802     F*---
002800990802     F* SEGNACOLLI
002900990802     F*---
003000990802     FTITAT30C  UF   E           K DISK
003100990802     FTITATP0F  O  A E           K DISK
003200990802     F                                     RENAME(TITATP00:TITATS00)
003300990802     F*---
003400990802     F* RIFERIMENTI
003500990802     F*---
003600990802     FTITA430C  UF   E           K DISK
003700990802     FTITA4P0F  O  A E           K DISK
003800990802     F                                     RENAME(TITA4P00:TITA4S00)
003900010903     F*---
004000010903     F* ESTENSIONE DETTAGLI SEGNACOLLI
004100010903     F*---
004200010903     FTITAH30C  UF   E           K DISK
004300010903     FTITAHP0F  O  A E           K DISK
004400010903     F                                     RENAME(TITAHP00:TITAHS00)
004500990802     F*---
004600990802     F* COMPETENZE TESTATA
004700990802     F*---
004800950307     FECCET30C  UF   E           K DISK
004900950307     FECCETP0F  O  A E           K DISK
005000010829     F                                     RENAME(ECCETP00:ECCETS)
005100990802     F*---
005200990802     F* COMPETENZE DETTAGLIO
005300990802     F*---
005400950307     FECCED30C  UF   E           K DISK
005500950307     FECCEDP0F  O  A E           K DISK
005600010829     F                                     RENAME(ECCEDP00:ECCEDS)
005700040220     F*---
005800990802     F* INFRAGRUPPO METRICUBI
005900990802     F*---
006000980325     FECFIM30C  UF   E           K DISK
006100980325     FECFIMP0F  O  A E           K DISK
006200980325     F                                     RENAME(ECFIMP00:ECFIMS00)
006300990802     F*---
006400990802     F* VOCI COMPETENZE TESTATA
006500990802     F*---
006600970903     FECECT01L  UF   E           K DISK
006700970903     FECECT00F  O  A E           K DISK
006800970903     F                                     RENAME(ECECT:ECECTS)
006900990802     F*---
007000990802     F* VOCI COMPETENZE DETTAGLIO
007100990802     F*---
007200970903     FECECD01L  UF   E           K DISK
007300970903     FECECD00F  O  A E           K DISK
007400970903     F                                     RENAME(ECECD:ECECDS)
007500000327     F*---
007600000327     F* EVENTI
007700000327     F*---
007800000327     FFNEVB01L  UF   E           K DISK
007900000327     FFNEVB00F  O  A E           K DISK
008000000327     F                                     RENAME(FNEVB000:FNEVBS)
008100000404     F*---
008200000404     F* GIACENZE
008300000404     F*---
008400050301     Ftigcp51l  UF   E           K DISK
008500050301     Ftigcp00f  O  A E           K DISK
008600050301     F                                     RENAME(tigcp000:tigcpS)
008700000404     F*---
008800000404     F* GIACENZE -ANAGRAFICHE MITTENTE-
008900000404     F*---
009000050301     Ftigap01l  UF   E           K DISK
009100050301     Ftigap00f  O  A E           K DISK
009200050301     F                                     RENAME(TIGAP000:TIGAPS)
009300000404     F*---
009400000404     F* GIACENZE -NOTE-
009500000404     F*---
009600050301     FTIGNP01L  UF   E           K DISK
009700050301     FTIGNP00F  O  A E           K DISK
009800050301     F                                     RENAME(TIGNP000:TIGNPS)
009900040303     F*---
010000040303     F* ESTENSIONE ALLA TESTATA BOLLE
010100040303     F*---
010200040531     FFIAR531C  UF   E           K DISK
010300040303     FFIAR5P0F  O  A E           K DISK
010400040303     F                                     RENAME(FIAR5P00:FIAR5S)
010500060919     F*---
010600060919     Ftitai01l  UF   E           K DISK
010700060919     Ftitai00F  O  A E           K DISK
010800060919     F                                     RENAME(titai000:titaiS)
010900081103     F*---
011000081103     Ffirab01l  UF   E           K DISK
011100081103     Ffirab00F  O  A E           K DISK
011200081103     F                                     RENAME(firab000:firabS)
011300061122     F*---
011400061122     Ffiapd01l  IF   E           K DISK
011500080312     Ftilad01l  IF   E           K DISK
011600000404     F*---
011700000327     F* TABELLE
011800000327     F*---
011900950309     FTABEL00F  UF   E           K DISK    USROPN
012000000327     F*---
012100000327     F* CONTRASSEGNI
012200000327     F*---
012300950309     FTNCSB03L  IF   E           K DISK
012400000327     F*---
012500000327     F* DANNI
012600000327     F*---
012700040715     F***FNDCT02L  IF   E           K DISK
012800040823     F*---
012900040823     Ffnblp01L  IF   E           K DISK    usropn
013000040823     Ffnarb01L  IF   E           K DISK    usropn
013100000327     F*---
013200000327     F* PRINTER FILE
013300000327     F*---
013400931105     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
013500931105     F                                     FORMLEN(66)
013600931105     F                                     FORMOFL(60)
013700940912     L*------------------------------------------------------------------------*
013800040823     d wlib            s             10    inz('FILTRA201 ')
013900040910     d wlibp           s             10    inz('FILTRAPRD ')
014000040823     d comman          s             80
014100040823     d lenght          s             15  5 inz(80)
014200061122     d trcapd          s                   like(apdtip) inz('A')
014300061122     d com04           s              4  0
014400940912     D*------------------------------------------------------------------------*
014500940912     D* SCHIERE
014600940912     D*------------------------------------------------------------------------*
014700040823     d cmd             s             60    dim(01) ctdata perrcd(1)
014800931105     D TES             S             66    DIM(15) CTDATA PERRCD(1)
014900080925     d tbl             s              2    dim(50) inz
015000950309     D* DS TABELLA $3 (DATE AVANZAMENTO TRASFERIMENTO BOLLE)
015100950309     D DS$3          E DS
015200080925     D DStb          E DS
015300950717     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
015400950717     D DS§DAT          DS
015500950717     D  DSSEC                  1      2  0
015600950717     D  DSANN                  3      4  0
015700950717     D  DSMES                  5      6  0
015800950717     D  DSGIO                  7      8  0
015900950717     D  DSAAS                  1      4  0
016000950717     D  DSMGS                  5      8  0
016100950717     D  DSDAT6                 3      8  0
016200950717     D  DSDAT8                 1      8  0
016300950717     D DS§TIM          DS
016400950717     D  DTORA                  1      2  0
016500950717     D  DTMIN                  3      4  0
016600950717     D  DTSCN                  5      6  0
016700950717     D  DTGIO                  7      8  0
016800950717     D  DTMES                  9     10  0
016900950717     D  DTSEC                 11     12  0
017000950717     D  DTANN                 13     14  0
017100950717     D  DTORA6                 1      6  0
017200950717     D  DTTIME                 1     14  0
017300940912     D*------------------------------------------------------------------------*
017400940912     D* INPUT
017500940912     D*------------------------------------------------------------------------*
017600941123     D                 DS
017700941123     D  DSAASI                 1      4  0
017800941123     D  DSMGSI                 5      8  0
017900950309     D  DSDATI                 1      8  0
018000941123     D                 DS
018100941123     D  DSAASF                 1      4  0
018200941123     D  DSMGSF                 5      8  0
018300950309     D  DSDATF                 1      8  0
018400020220     D                 DS
018500020220     D  DSAASFP                1      4  0
018600020220     D  DSMGSFP                5      8  0
018700020220     D  DSDATFP                1      8  0
018800960413     D*-------------------
018900960413     D* DATA SPEDIZIONE
019000960413     D*-------------------
019100960413     D                 DS
019200960413     D  TASAAS                 1      4  0
019300960413     D  TASMGS                 5      8  0
019400960413     D  TASDSP                 1      8  0
019500000327     D*------------------
019600000327     D* DS "XSRDA8" - CONTROLLA DATA (8)
019700000327     D*------------------
019800000327     D WLBDA8          DS                  INZ
019900000327     D  G08DAT                 1      8  0
020000000327     D  G08INV                 9     16  0
020100000327     D  G08ERR                17     17
020200000327     D  G08TGI                18     22  0
020300040715
020400040715     d Fidn12ds      e ds
020500040715     d  skKey                 26    305    dim(20)
020600040715     d  skAnn                306    325    dim(20)
020700040715     d  skDca                326    485    dim(20)
020800040715     d  skFca                486    545  0 dim(20)
020900040715     d  skDch                546    705  0 dim(20)
021000040715     d  skCch                706    745    dim(20)
021100040715
021200940912     D*-------------------
021300931115     D* PARAMETRI LANCIO PGM
021400940912     D*-------------------
021500000327     D PARAM           DS
021600950309     D  PARDTI                 1      8  0                                      DT.INI
021700950309     D  PARDTF                 9     16  0                                      DT.FIN
021800020220     D  PARDFP                17     24  0                                      DT.FIN PT
021900050706     D  PARcop                25     27                                         DT.FIN PT
022000940912     D*-------------------
022100940912     D* ARCHITETTURA
022200940912     D*-------------------
022300931115     D KPJBA         E DS
022400050705     D tnsb34ds      E DS
022500050705     D dtasflo       E DS
022600061121      *
022700061121     D*-------------------
022800061121     D* ANAGRAFICA
022900061121     D*-------------------
023000061121     d TIBS69ds      e ds                  inz
023100061121     d DS_cnaco      e ds                  inz  extname(CNACO00F)
023200061121     d DS_cnind      e ds                  inz  extname(CNIND00F)
023300061121     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
023400061121     d DS_fncls      e ds                  inz  extname(FNCLS00F)
023500061122     D*-------------------
023600061122     D* RIFERIMENTI ALFANUMERICI
023700061122     D*-------------------
023800061122     d DTA4A         e ds
023900061122     D*-------------------
024000061122     D* CODICE FISCALE
024100061122     D*-------------------
024200061122     d DTA4Q         e ds
024300940912     C*------------------------------------------------------------------------*
024400940912     C*?MAIN LINE
024500940912     C*------------------------------------------------------------------------*
024600000000     C*
024700930129     C* ROUTINE INIZIALE
024800930129     C                   EXSR      RUTINZ
024900931105     C*
025000931105     C* STAMPA RIGA DI TESTATA
025100931105     C                   EXCEPT    TESTA
025200930601     C*
025300930601     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
025400930601     C     FINL00        IFEQ      '0'                                          --- 1 -->
025500000000     C*
025600990802     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA SU TITAS
025700950307     C                   EXSR      SETTAS
025800930128     C*
025900990802     C* CICLO FINO A FINE FILE TITAS OPPURE
026000950308     C* RICHIESTA CHIUSURA PGM OPPURE
026100950308     C* FINE PGM PER ERRORE
026200950308     C                   SETOFF                                       98
026300950307     C     FINTAS        DOWEQ     '0'                                          --- 2 -->
026400950308     C     *IN98         ANDEQ     '0'
026500950308     C     FINL00        ANDEQ     '0'
026600080214      * verifico   che non esistano ARB e BLP altrimenti non cancello
026700040823      * e leggo un'altro rekord
026800040823     C     Kbol          Chain     fnblp01l                           33
026900040922     C  N33              ADD       1             NUMblp                         *REC TRASF TITA7
027000110408     C**n33              EXCEPT    ERRblp
027100080214     C     kbol          Chain     fnarb01l                           34
027200080214     C  N34              ADD       1             NUMarb                         *REC TRASF TITA7
027300110408     C**n34              EXCEPT    ERRarb
027400080214     c                   if        not *in33 or not *in34
027500080214     c                   goto      altrorek
027600080214     c                   end
027700950307     C*
027800001019     C                   WRITE     TITASS00                             99      *SCRIVO TITASS
027900001019     C  N99              DELETE    TITASP00                             99      *CANCELLO TITASP
028000950307     C   99              EXCEPT    ERRTAS
028100950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
028200990802     C  N99              ADD       1             NUMTAS                         *REC TRASF TITAS
028300050705     c* se bolla ha immagine di laguna scrivo record sul file TILAD00F
028400050705     c                   if        not *in99
028500050705     c                   movel     tasflo        dtasflo
028600050705     c                   if        §floiml <> ' '
028700050705     c                   exsr      srlaguna
028800050705     c                   end
028900050705     c                   end
029000950307     C*
029100990802     C* SE ESISTE TITA7 TRASFERISCO ANCHE QUELLO
029200950307     C     TASSV3        IFNE      *BLANKS                                      --- 3 -->
029300990802     C                   EXSR      SETTA7                                       *PRIMO TITA7
029400950307     C     FINTA7        DOWEQ     '0'                                          --- 4 -->
029500990802     C                   WRITE     TITA7S00                             99      *SCRIVO TITA7S
029600001019     C  N99              DELETE    TITA730C                             99      *CANCELLO TITA730C
029700950307     C   99              EXCEPT    ERRTA7
029800950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
029900990802     C  N99              ADD       1             NUMTA7                         *REC TRASF TITA7
030000990802     C                   EXSR      LETTA7                                       *SUCCESSIVO TITA7
030100950307     C                   ENDDO                                                  <-- 4 ---
030200950307     C                   ENDIF                                                  <-- 3 ---
030300990802     C*
030400990802     C* TRASFERISCE TITAA - ANAGRAFICHE
030500990802     C                   EXSR      SETTAA                                       *PRIMO TITAA
030600990802DO  3C     FINTAA        DOWEQ     '0'
030700990802     C                   WRITE     TITAAS00                             99      *SCRIVO TITAAS
030800001019     C  N99              DELETE    TITAA30C                             99      *CANCELLO TITAA30C
030900990802     C   99              EXCEPT    ERRTAA
031000990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
031100990802     C  N99              ADD       1             NUMTAA                         *REC TRASF TITAA
031200990802     C                   EXSR      LETTAA                                       *SUCCESSIVO TITAA
031300990802E   3C                   ENDDO
031400061121     C* verifico se scritto i dati anagrafici del mittente altrimenti aggancio anagrafica
031500061121     c                   if        tasrsm = *blanks
031600061121     c                   exsr      recana
031700061121     c                   endif
031800990802     C*
031900990802     C* TRASFERISCE TITAT - SEGNACOLLI
032000990802     C                   EXSR      SETTAT                                       *PRIMO TITAT
032100990802DO  3C     FINTAT        DOWEQ     '0'
032200990802     C                   WRITE     TITATS00                             99      *SCRIVO TITATS
032300001019     C  N99              DELETE    TITAT30C                             99      *CANCELLO TITAT30C
032400990802     C   99              EXCEPT    ERRTAT
032500990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
032600990802     C  N99              ADD       1             NUMTAT                         *REC TRASF TITAT
032700990802     C                   EXSR      LETTAT                                       *SUCCESSIVO TITAT
032800990802E   3C                   ENDDO
032900990802     C*
033000990802     C* TRASFERISCE TITA4 - RIFERIMENTI
033100990802     C                   EXSR      SETTA4                                       *PRIMO TITA4
033200990802DO  3C     FINTA4        DOWEQ     '0'
033300990802     C                   WRITE     TITA4S00                             99      *SCRIVO TITA4S
033400001019     C  N99              DELETE    TITA430C                             99      *CANCELLO TITA430C
033500990802     C   99              EXCEPT    ERRTA4
033600990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
033700990802     C  N99              ADD       1             NUMTA4                         *REC TRASF TITA4
033800990802     C                   EXSR      LETTA4                                       *SUCCESSIVO TITA4
033900990802E   3C                   ENDDO
034000010903     C*
034100010903     C* TRASFERISCE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
034200010903     C                   EXSR      SETTAH                                       *PRIMO TITAH
034300010903DO  3C     FINTAH        DOWEQ     '0'
034400010903     C                   WRITE     TITAHS00                             99      *SCRIVO TITAHS
034500010903     C  N99              DELETE    TITAH30C                             99      *CANCELLO TITAH30C
034600010903     C   99              EXCEPT    ERRTAH
034700010903     C   99              MOVEL     '1'           FINL00                         *FINE PGM
034800010903     C  N99              ADD       1             NUMTAH                         *REC TRASF TITAH
034900010903     C                   EXSR      LETTAH                                       *SUCCESSIVO TITAH
035000010903E   3C                   ENDDO
035100950307     C*
035200950307     C* SE ESISTE ECCET LO TRASFERISCE
035300971008     C                   Z-ADD     999           MINNBC                         MIMIMO BLOCCO
035400950307     C                   Z-ADD     TASAAS        CETAAS                         ANNO SPEDIZIONE
035500950307     C                   Z-ADD     TASLNP        CETLNP                         LINEA DI PARTENZA
035600950307     C                   Z-ADD     TASNRS        CETNRS                         NUMERO SERIE
035700950307     C                   Z-ADD     TASNSP        CETNSP                         NUMERO SPEDIZIONE
035800950307     C                   MOVEL     TASTBL        CETTBL                         TIPO BOLLA
035900950307     C     KEYCET        CHAIN     ECCET30C                           99
036000950307     C     *IN99         IFEQ      *OFF                                         --- 3 --> *TROVATO
036100970903     C*
036200970903     C* MEMORIZZA IL NUMERO BLOCCO COMPETENZE TESTATA PIU' PICCOLO
036300971008IF  4C     CETNBC        IFGT      *ZEROS
036400971008IF  5C     CETNBC        IFLT      MINNBC
036500970903     C                   Z-ADD     CETNBC        MINNBC
036600971008E   5C                   ENDIF
036700971008E   4C                   ENDIF
036800970903     C*
036900950309     C                   WRITE     ECCETS                               99      *SCRIVO ECCETS
037000001019     C  N99              DELETE    ECCET30C                             99      *CANCELLO ECCET30C
037100950307     C   99              EXCEPT    ERRCET
037200950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
037300950308     C  N99              ADD       1             NUMCET                         *REC TRASF ECCET
037400950307     C*
037500950307     C* SE ESISTE ECCED TRASFERISCO ANCHE QUELLO
037600950307     C     CETDET        IFEQ      'S'                                          --- 4 -->
037700950307     C                   EXSR      SETCED                                       *PRIMO ECCED
037800950307     C     FINCED        DOWEQ     '0'                                          --- 5 -->
037900950309     C                   WRITE     ECCEDS                               99      *SCRIVO ECCEDS
038000001020     C  N99              DELETE    ECCED30C                             99      *CANCELLO ECCED30C
038100950307     C   99              EXCEPT    ERRCED
038200950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
038300950308     C  N99              ADD       1             NUMCED                         *REC TRASF ECCED
038400950307     C                   EXSR      LETCED                                       *SUCCESSIVO ECCED
038500950307     C                   ENDDO                                                  <-- 5 ---
038600950307     C                   ENDIF                                                  <-- 4 ---
038700950307     C                   ENDIF                                                  <-- 3 ---
038800980325     C*
038900980325     C* SE ESISTE ECFIM LO TRASFERISCE
039000980325     C                   Z-ADD     TASAAS        FIMAAS                         ANNO SPEDIZIONE
039100980325     C                   Z-ADD     TASLNP        FIMLNP                         LINEA DI PARTENZA
039200980325     C                   Z-ADD     TASNRS        FIMNRS                         NUMERO SERIE
039300980325     C                   Z-ADD     TASNSP        FIMNSP                         NUMERO SPEDIZIONE
039400980325     C                   MOVEL     TASTBL        FIMTBL                         TIPO BOLLA
039500001019     C     KEYFIM        CHAIN     ECFIM30C                           99
039600980325     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
039700980325     C                   WRITE     ECFIMS00                             99      *SCRIVE
039800001019     C  N99              DELETE    ECFIM30C                             99      *CANCELLA
039900980325     C   99              EXCEPT    ERRFIM
040000980325     C   99              MOVEL     '1'           FINL00                         *FINE PGM
040100980325     C  N99              ADD       1             NUMFIM                         *REC TRASF ECCET
040200011127     C     KEYFIM        READE     ECFIM30C                               99
040300980325     C                   ENDDO                                                  <-- 3 ---
040400000327     C*
040500000327     C* SE ESISTE FNEVB LO TRASFERISCE
040600000327     C                   Z-ADD     TASAAS        EVBAAS                         ANNO SPEDIZIONE
040700000327     C                   Z-ADD     TASLNP        EVBLNP                         LINEA DI PARTENZA
040800000327     C                   Z-ADD     TASNRS        EVBNRS                         NUMERO SERIE
040900000327     C                   Z-ADD     TASNSP        EVBNSP                         NUMERO SPEDIZIONE
041000001019     C     KEYEVB        CHAIN     FNEVB01L                           99
041100000327     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
041200000327     C                   WRITE     FNEVBS                               99      *SCRIVE
041300001019     C  N99              DELETE    FNEVB01L                             99      *CANCELLA
041400000327     C   99              EXCEPT    ERREVB
041500000327     C   99              MOVEL     '1'           FINL00                         *FINE PGM
041600000327     C  N99              ADD       1             NUMEVB                         *REC TRASF FNEVB
041700000327     C     KEYEVB        READE     FNEVB01L                               99
041800000327     C                   ENDDO                                                  <-- 3 ---
041900000404     C*
042000050301     C* SE ESISTE TIGCP LO TRASFERISCE
042100001018     C     TASFGC        IFEQ      'S'
042200000404     C                   Z-ADD     TASAAS        KCPAAS                         ANNO SPEDIZIONE
042300000404     C                   Z-ADD     TASLNP        KCPLNP                         LINEA DI PARTENZA
042400000404     C                   Z-ADD     TASNRS        KCPNRS                         NUMERO SERIE
042500000404     C                   Z-ADD     TASNSP        KCPNSP                         NUMERO SPEDIZIONE
042600050301     C     KE2GCP        CHAIN     TIGCP51L                           99
042700001018     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
042800050303     c     gcpsede       ifeq      'S'
042900050301     C                   WRITE     TIGCPS                               99      *SCRIVE
043000050301     C  N99              DELETE    TIGCP51L                             99      *CANCELLA
043100000404     C   99              EXCEPT    ERRGCP
043200000404     C   99              MOVEL     '1'           FINL00                         *FINE PGM
043300000404     C  N99              ADD       1             NUMGCP                         *REC TRASF FNEVB
043400000404     C*
043500050301     C* SE ESISTE TIGAP LO TRASFERISCE
043600000404     C                   Z-ADD     GCPAAS        KAPAAS                         ANNO SPEDIZIONE
043700000404     C                   Z-ADD     GCPLNP        KAPLNP                         LINEA DI PARTENZA
043800000404     C                   Z-ADD     GCPNRS        KAPNRS                         NUMERO SERIE
043900000404     C                   Z-ADD     GCPNSP        KAPNSP                         NUMERO SPEDIZIONE
044000050301     C     KEYGAP        CHAIN     TIGAP01L                           99
044100001018     C     *IN99         DOWEQ     *OFF
044200050301     C                   WRITE     TIGAPS                               99      *SCRIVE
044300050301     C  N99              DELETE    TIGAP01L                             99      *CANCELLA
044400001017     C   99              EXCEPT    ERRGAP
044500001017     C   99              MOVEL     '1'           FINL00                         *FINE PGM
044600001017     C  N99              ADD       1             NUMGAP                         *REC TRASF FNEVB
044700050301     C     KEYGAP        READE     TIGAP01L                               99
044800001018     C                   ENDDO
044900000404     C*
045000050301     C* SE ESISTE TIGNP LO TRASFERISCE
045100000404     C                   Z-ADD     GCPAGC        KNPAGC                         ANNO APERTURA
045200000404     C                   Z-ADD     GCPFGC        KNPFGC                         FILIALE APERTURA
045300000404     C                   Z-ADD     GCPNGC        KNPNGC                         NUMERO GIACENZA
045400050301     C     KEYGNP        CHAIN     TIGNP01L                           99
045500001018     C     *IN99         DOWEQ     *OFF
045600050301     C                   WRITE     TIGNPS                               99      *SCRIVE
045700050301     C  N99              DELETE    TIGNP01L                             99      *CANCELLA
045800001017     C   99              EXCEPT    ERRGNP
045900001017     C   99              MOVEL     '1'           FINL00                         *FINE PGM
046000001017     C  N99              ADD       1             NUMGNP                         *REC TRASF FNEVB
046100050301     C     KEYGNP        READE     TIGNP01L                               99
046200001018     C                   ENDDO
046300050303     c                   else
046400050303     C                   EXCEPT    ERRGCP1
046500060707     C*                  MOVEL     '1'           FINL00                         *FINE PGM
046600050303     C                   ENDIF
046700000404     C*
046800000404     C* LETTURA SUCCESSIVA GIACENZE
046900050301     C     KE2GCP        READE     TIGCP51L                               99
047000001018     C                   ENDDO
047100001018     C                   ENDIF
047200040303     C*
047300040303     C* SE ESISTE FIAR5 LO TRASFERISCE
047400040303     C                   Z-ADD     TASAAS        AR5AAS                         ANNO SPEDIZIONE
047500040303     C                   Z-ADD     TASLNP        AR5LNP                         LINEA DI PARTENZA
047600040303     C                   Z-ADD     TASNRS        AR5NRS                         NUMERO SERIE
047700040303     C                   Z-ADD     TASNSP        AR5NSP                         NUMERO SPEDIZIONE
047800040531     C     KEYAR5        CHAIN     FIAR531C                           99
047900040303     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
048000040303     C                   WRITE     FIAR5S                               99      *SCRIVE
048100040531     C  N99              DELETE    FIAR531C                             99      *CANCELLA
048200040303     C   99              EXCEPT    ERRAR5
048300040303     C   99              MOVEL     '1'           FINL00                         *FINE PGM
048400040303     C  N99              ADD       1             NUMAR5                         *REC TRASF FNEVB
048500040531     C     KEYAR5        READE     FIAR531C                               99
048600040303     C                   ENDDO                                                  <-- 3 ---
048700060919     C*
048800060919     C* SE ESISTE TITAI LO TRASFERISCE
048900060919     C                   Z-ADD     TASAAS        taiAAS                         ANNO SPEDIZIONE
049000060919     C                   Z-ADD     TASLNP        taiLNP                         LINEA DI PARTENZA
049100060919     C                   Z-ADD     TASNRS        taiNRS                         NUMERO SERIE
049200060919     C                   Z-ADD     TASNSP        taiNSP                         NUMERO SPEDIZIONE
049300060919     C                   movel     TASTBL        taiTBL                         NUMERO SPEDIZIONE
049400060919     C     KEYtai        CHAIN     titai01l                           99
049500060919     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
049600060919     C                   WRITE     titaiS                               99      *SCRIVE
049700060919     C  N99              DELETE    titai01l                             99      *CANCELLA
049800060919     C   99              EXCEPT    ERRtai
049900060919     C   99              MOVEL     '1'           FINL00                         *FINE PGM
050000060919     C  N99              ADD       1             NUMtai                         *REC TRASF FNEVB
050100060919     C     KEYtai        READE     titai01l                               99
050200060919     C                   ENDDO                                                  <-- 3 ---
050300081103     C* SE ESISTE FIRAB LO TRASFERISCE
050400081103     C                   Z-ADD     TASAAS        rabAAS                         ANNO SPEDIZIONE
050500081103     C                   Z-ADD     TASLNP        rabLNP                         LINEA DI PARTENZA
050600081103     C                   Z-ADD     TASNRS        rabNRS                         NUMERO SERIE
050700081103     C                   Z-ADD     TASNSP        rabNSP                         NUMERO SPEDIZIONE
050800081103     C     KEYrab        CHAIN     firab01l                           99
050900081103     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
051000081103     C                   WRITE     firabS                               99      *SCRIVE
051100081103     C  N99              DELETE    firab01l                             99      *CANCELLA
051200081103     C   99              EXCEPT    ERRrab
051300081103     C   99              MOVEL     '1'           FINL00                         *FINE PGM
051400081103     C  N99              ADD       1             NUMrab                         *REC TRASF FNEVB
051500081103     C     KEYrab        READE     firab01l                               99
051600081103     C                   ENDDO                                                  <-- 3 ---
051700061214      * decodifico alcuni dati per il nuovo storico  e lo scrivo se non bolla Poste
051800061214     c                   If        tastsp <> 'P'
051900080926     c* se bolla di recupero non scrivo storico
052000080926     c                   z-add     1             conta
052100080926     c     tastbl        lookup    tbl(conta)                             44
052200080926     c                   if        not *in44
052300061122     c                   EXSR      wrttassto
052400061214     c                   endif
052500080926     c                   endif
052600061122
052700000000     C*
052800980325     C* LETTURA SUCCESSIVA BOLLE
052900040823     c     altrorek      tag
053000040823
053100060707     c                   if        finl00 = '0'
053200990802     C                   EXSR      LETTAS                                       *SUCCESSIVO TITAS
053300060707     c                   end
053400950308     C                   SHTDN                                        98        *CONTR.CHIUSURA PGM
053500930128     C*
053600950307     C                   ENDDO                                                  <-- 2 ---
053700950307     C                   ENDIF                                                  <-- 1 ---
053800970903     C*
053900970903     C* PULISCE I CODICI COMPETENZA -TESTATA E DETTAGLIO- CON NUMERO BLOCCO
054000970903     C* MINORE DI QUELLO PIU' PICCOLO CON CUI SONO STATE CALCOLATE LE BOLLE
054100971008IF  1C     MINNBC        IFNE      999
054200970903     C     *LOVAL        SETLL     ECECT01L
054300970903     C                   READ      ECECT01L                               97
054400971008DO  2C     *IN97         DOWEQ     *OFF
054500971008IF  3C     ECTATB        IFEQ      *BLANKS
054600971008IF  4C     ECTNBC        IFLT      MINNBC
054700970903     C                   MOVEL     'A'           ECTATB
054800970903     C                   MOVEL     *BLANKS       ECTFTR
054900970903     C                   EXCEPT    AGGECT                                       *AGGIORNAMENTO CAMPI
055000970903     C                   WRITE     ECECTS                               97      *SCRIVO ECECTS
055100970903     C   97              EXCEPT    ERRECT                                       *SCRIVE ERRORE
055200970903     C   97              MOVEL     '1'           FINL00                         *FINE PGM
055300970903     C  N97              ADD       1             NUMECT                         *REC TRASF ECECT
055400971008E   4C                   ENDIF
055500971008E   3C                   ENDIF
055600970903     C                   READ      ECECT01L                               97
055700971008E   2C                   ENDDO
055800950309     C*
055900970903     C     *LOVAL        SETLL     ECECD01L
056000970903     C                   READ      ECECD01L                               97
056100971008DO  2C     *IN97         DOWEQ     *OFF
056200971008IF  3C     ECDATB        IFEQ      *BLANKS
056300971008IF  4C     ECDNBC        IFLT      MINNBC
056400970903     C                   MOVEL     'A'           ECDATB
056500970903     C                   MOVEL     *BLANKS       ECDFTR
056600970903     C                   EXCEPT    AGGECD                                       *AGGIORNAMENTO CAMPI
056700970903     C                   WRITE     ECECDS                               97      *SCRIVO ECECDS
056800970903     C   97              EXCEPT    ERRECD                                       *SCRIVE ERRORE
056900970903     C   97              MOVEL     '1'           FINL00                         *FINE PGM
057000970903     C  N97              ADD       1             NUMECD                         *REC TRASF ECECD
057100971008E   4C                   ENDIF
057200971008E   3C                   ENDIF
057300970903     C                   READ      ECECD01L                               97
057400971008E   2C                   ENDDO
057500971008E   1C                   ENDIF
057600970903     C*
057700950309     C* AGGIORNA LA TABELLA CON L'ULTIMA DATA DI TRASFERIM. (SOLO SE TUTTO OK)
057800950309     C     *IN98         IFEQ      '0'
057900950309     C     FINL00        ANDEQ     '0'
058000950309     C                   EXSR      AGGTAB
058100950309     C                   ENDIF
058200931105     C*
058300931105     C* STAMPA RIGA DI FINE STAMPA
058400950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
058500931105     C                   EXCEPT    STAFIN
058600050705     c* chiudo il pgm di archiviazione Laguna
058700050705     C                   CLEAR                   Tnsb34ds
058800050705     C                   MOVEL     'C'           sb34tla
058900050705     C                   CALL      'TNSB34R'
059000050705     C                   PARM                    TNSB34ds
059100000000     C*
059200000000     C                   SETON                                        LR
059300940912     C*------------------------------------------------------------------------*
059400990802     C*? SETTAS - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAS
059500940912     C*------------------------------------------------------------------------*
059600950307     C     SETTAS        BEGSR
059700931115     C*
059800931115     C* AZZERO CONTATORE RECORD LETTI
059900950307     C                   MOVEL     '0'           FINTAS            1
060000930208     C*
060100930316     C* POSIZIONAMENTO E PRIMA LETTURA
060200941123     C                   Z-ADD     PARDTI        DSDATI
060300941123     C                   Z-ADD     PARDTF        DSDATF
060400020220     C                   Z-ADD     PARDFP        DSDATFP
060500941123     C                   Z-ADD     DSAASI        TASAAS
060600941123     C                   Z-ADD     DSMGSI        TASMGS
060700990802     C     KEYTAS        SETLL     TITAS38C                           99        *FINE-ARCHIV
060800930625     C*
060900930625     C     *IN99         IFEQ      '1'
061000950307     C                   MOVEL     '1'           FINTAS
061100930625     C                   ELSE
061200950307     C                   EXSR      LETTAS
061300001020     C                   ENDIF
061400930208     C*
061500930208     C                   ENDSR
061600940912     C*------------------------------------------------------------------------*
061700990802     C*? LETTAS - LETTURA PROSSIMO RECORD TITAS
061800940912     C*------------------------------------------------------------------------*
061900950307     C     LETTAS        BEGSR
062000930625     C*
062100930625     C* LEGGO FINO A:
062200930625     C*     - FINE FILE O SUPERATO LIMITE FINALE
062300930625     C*     - TROVATO RECORD VALIDO
062400930625     C                   MOVEL     'N'           WRECOK
062500950307     C     FINTAS        DOWEQ     '0'                                          --- 1 -->
062600930625     C     WRECOK        ANDEQ     'N'
062700061121      * pulisco lo storico
062800061121     c                   clear                   titassto
062900930208     C*
063000930316     C* LETTURE SUCCESSIVE ALLA PRIMA
063100990802     C                   READ      TITAS38C                               99
063200960413     C*
063300960413     C     *IN99         IFEQ      '1'                                          *FINE FILE
063400020220     C     TASDSP        ORGT      DSDATF                                       *SUPERATA DATA FINE
063500950307     C                   MOVEL     '1'           FINTAS
063600930625     C                   ELSE
063700950309     C                   EXSR      CHKTAS                                       *CONTR.VALIDITA REC.
063800001019     C                   ENDIF
063900930625     C*
064000001019     C                   ENDDO                                                  <-- 1 ---
064100930208     C*
064200930208     C                   ENDSR
064300950307     C*------------------------------------------------------------------------*
064400990802     C*? SETTA7 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA7
064500950307     C*------------------------------------------------------------------------*
064600950307     C     SETTA7        BEGSR
064700950307     C*
064800950307     C* AZZERO CONTATORE RECORD LETTI
064900950307     C                   MOVEL     '0'           FINTA7            1
065000950307     C*
065100950307     C* POSIZIONAMENTO E PRIMA LETTURA
065200950307     C                   Z-ADD     TASAAS        TA7AAS
065300950307     C                   Z-ADD     TASLNP        TA7LNP
065400950307     C                   Z-ADD     TASNRS        TA7NRS
065500950307     C                   Z-ADD     TASNSP        TA7NSP
065600950307     C                   MOVEL     TASTBL        TA7TBL
065700001019     C*
065800001019     C     KEYTA7        CHAIN     TITA730C                           99        *FINE-ARCHIV
065900950307     C     *IN99         IFEQ      '1'
066000950307     C                   MOVEL     '1'           FINTA7
066100001019     C                   ENDIF
066200950307     C*
066300950307     C                   ENDSR
066400950307     C*------------------------------------------------------------------------*
066500990802     C*? LETTA7 - LETTURA PROSSIMO RECORD TITA7
066600950307     C*------------------------------------------------------------------------*
066700001019     C     LETTA7        BEGSR
066800001020     C*
066900001020     C* LETTURE SUCCESSIVE ALLA PRIMA
067000010124     C                   READ      TITA730C                               99
067100950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
067200010124     C     TA7AAS        ORNE      TASAAS
067300010124     C     TA7LNP        ORNE      TASLNP
067400010124     C     TA7NRS        ORNE      TASNRS
067500010124     C     TA7NSP        ORNE      TASNSP
067600010124     C     TA7TBL        ORNE      TASTBL
067700950307     C                   MOVEL     '1'           FINTA7
067800001019     C                   ENDIF
067900950307     C*
068000950307     C                   ENDSR
068100990802     C*------------------------------------------------------------------------*
068200990802     C*? SETTAA - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAA - ANAGRAFICHE
068300990802     C*------------------------------------------------------------------------*
068400990802     C     SETTAA        BEGSR
068500990802     C*
068600990802     C* AZZERO CONTATORE RECORD LETTI
068700990802     C                   MOVEL     '0'           FINTAA            1
068800990802     C*
068900990802     C* POSIZIONAMENTO E PRIMA LETTURA
069000990802     C                   Z-ADD     TASAAS        TAAAAS
069100990802     C                   Z-ADD     TASLNP        TAALNP
069200990802     C                   Z-ADD     TASNRS        TAANRS
069300990802     C                   Z-ADD     TASNSP        TAANSP
069400001019     C     KEYTAA        CHAIN     TITAA30C                           99        *FINE-ARCHIV
069500990802     C     *IN99         IFEQ      '1'
069600990802     C                   MOVEL     '1'           FINTAA
069700061121      * recupero dati anagrafici del mittente
069800061121     c                   else
069900061121     c                   exsr      rectaa
070000001019     C                   ENDIF
070100990802     C*
070200990802     C                   ENDSR
070300990802     C*------------------------------------------------------------------------*
070400990802     C*? LETTAA - LETTURA PROSSIMO RECORD TITAA - ANAGRAFICHE
070500990802     C*------------------------------------------------------------------------*
070600990802     C     LETTAA        BEGSR
070700010124     C*
070800010124     C                   READ      TITAA30C                               99
070900990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
071000010130     C     TAAAAS        ORNE      TASAAS
071100010130     C     TAALNP        ORNE      TASLNP
071200010130     C     TAANRS        ORNE      TASNRS
071300010130     C     TAANSP        ORNE      TASNSP
071400990802     C                   MOVEL     '1'           FINTAA
071500001019     C                   ENDIF
071600061121      * se letto
071700061123     c                   if        fintaa = '0'
071800061121     c                   exsr      rectaa
071900061121     c                   endif
072000990802     C*
072100990802     C                   ENDSR
072200061121     C*------------------------------------------------------------------------*
072300061121     C*? RECTAA - RECUPERO DATI ANAGRAFICI MITTENTE
072400061121     C*------------------------------------------------------------------------*
072500061121     C     RECTAA        BEGSR
072600061121      * dati anagrafici mittente
072700061121     c                   IF        taatrc = 'M'
072800061121     c                   eval      tasrsm = taarsc
072900061121     c                   eval      tasinm = taaind
073000061121     c                   eval      tascam = taacap
073100061121     c                   eval      taslom = taaloc
073200061121     c                   eval      tasprm = taaprv
073300061121     c                   eval      tasnzm = taanaz
073400061122      * iva  per i franchi non coidficati
073500061122     c                   if        %subst(tastbl:1:1) = 'F'
073600061122     c                   eval      tasiva = taacpi
073700061122     c                   endif
073800061122
073900061121     c                   endif
074000061121
074100061121     c                   endsr
074200061121     C*------------------------------------------------------------------------*
074300061121     C*? RECANA - RECUPERO DA ANAGRAFICO CLIENTE
074400061121     C*------------------------------------------------------------------------*
074500061121     C     RECANA        BEGSR
074600061121     C                   clear                   ds_CNACO
074700061121     C                   clear                   ds_CNIND
074800061121     C                   clear                   ds_CNCLP
074900061121     C                   clear                   ds_FNCLS
075000061121     c                   z-add     TASccm        I69kac
075100061121     c                   z-add     TASccm        I69kin
075200061121     c                   call      'TIBS69R'
075300061121     c                   parm                    TIBS69ds
075400061121     c                   parm                    ds_CNACO
075500061121     c                   parm                    ds_CNIND
075600061121     c                   parm                    ds_CNCLP
075700061121     c                   parm                    ds_FNCLS
075800061122if  3c                   if        O69err <> *on
075900061122     c                   movel     ACOrag        TASrsm
076000061122     c                   movel     INDvia        TASinm
076100061122     c                   movel     INDcap        TAScam
076200061122     c                   movel     INDcit        TASlom
076300061122     c                   movel     INDprv        TASprm
076400061122     c                   movel     INDsta        TASnzm
076500061122e   3c                   endif
076600061121
076700061121     c                   endsr
076800061122     C*------------------------------------------------------------------------*
076900061122     C*? RECIVA - RECUPERO DA ANAGRAFICO CLIENTE IVA E/O CODICE FISCALE
077000061122     C*------------------------------------------------------------------------*
077100061122     C     RECIVA        BEGSR
077200061122     C                   clear                   ds_CNACO
077300061122     C                   clear                   ds_CNIND
077400061122     C                   clear                   ds_CNCLP
077500061122     C                   clear                   ds_FNCLS
077600061122     c                   z-add     TASKSC        I69kin
077700061122     c                   call      'TIBS69R'
077800061122     c                   parm                    TIBS69ds
077900061122     c                   parm                    ds_CNACO
078000061122     c                   parm                    ds_CNIND
078100061122     c                   parm                    ds_CNCLP
078200061122     c                   parm                    ds_FNCLS
078300061122
078400061122     c                   endsr
078500990802     C*------------------------------------------------------------------------*
078600990802     C*? SETTAT - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAT - SEGNACOLLI
078700990802     C*------------------------------------------------------------------------*
078800990802     C     SETTAT        BEGSR
078900990802     C*
079000990802     C* AZZERO CONTATORE RECORD LETTI
079100990802     C                   MOVEL     '0'           FINTAT            1
079200990802     C*
079300990802     C* POSIZIONAMENTO E PRIMA LETTURA
079400990802     C                   Z-ADD     TASAAS        TATAAS
079500990802     C                   Z-ADD     TASLNP        TATLNP
079600990802     C                   Z-ADD     TASNRS        TATNRS
079700990802     C                   Z-ADD     TASNSP        TATNSP
079800001019     C     KEYTAT        CHAIN     TITAT30C                           99        *FINE-ARCHIV
079900990802     C     *IN99         IFEQ      '1'
080000990802     C                   MOVEL     '1'           FINTAT
080100001019     C                   ENDIF
080200990802     C*
080300990802     C                   ENDSR
080400990802     C*------------------------------------------------------------------------*
080500990802     C*? LETTAT - LETTURA PROSSIMO RECORD TITAT - SEGNACOLLI
080600990802     C*------------------------------------------------------------------------*
080700990802     C     LETTAT        BEGSR
080800010124     C*
080900010124     C                   READ      TITAT30C                               99
081000990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
081100010124     C     TATAAS        ORNE      TASAAS
081200010124     C     TATLNP        ORNE      TASLNP
081300010124     C     TATNRS        ORNE      TASNRS
081400010124     C     TATNSP        ORNE      TASNSP
081500990802     C                   MOVEL     '1'           FINTAT
081600001019     C                   ENDIF
081700990802     C*
081800990802     C                   ENDSR
081900990802     C*------------------------------------------------------------------------*
082000990802     C*? SETTA4 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA4 - RIFERIMENTI
082100990802     C*------------------------------------------------------------------------*
082200990802     C     SETTA4        BEGSR
082300990802     C*
082400990802     C* AZZERO CONTATORE RECORD LETTI
082500990802     C                   MOVEL     '0'           FINTA4            1
082600990802     C*
082700990802     C* POSIZIONAMENTO E PRIMA LETTURA
082800990802     C                   Z-ADD     TASAAS        TA4AAS
082900990802     C                   Z-ADD     TASLNP        TA4LNP
083000990802     C                   Z-ADD     TASNRS        TA4NRS
083100990802     C                   Z-ADD     TASNSP        TA4NSP
083200001019     C     KEYTA4        CHAIN     TITA430C                           99        *FINE-ARCHIV
083300990802     C     *IN99         IFEQ      '1'
083400990802     C                   MOVEL     '1'           FINTA4
083500061122      * recupero riferimento mittente alfanumerico
083600061122     c                   else
083700061122     c                   exsr      recta4
083800001019     C                   ENDIF
083900990802     C*
084000990802     C                   ENDSR
084100990802     C*------------------------------------------------------------------------*
084200990802     C*? LETTA4 - LETTURA PROSSIMO RECORD TITA4 - RIFERIMENTI
084300990802     C*------------------------------------------------------------------------*
084400990802     C     LETTA4        BEGSR
084500010124     C*
084600010124     C                   READ      TITA430C                               99
084700990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
084800010124     C     TA4AAS        ORNE      TASAAS
084900010124     C     TA4LNP        ORNE      TASLNP
085000010124     C     TA4NRS        ORNE      TASNRS
085100010124     C     TA4NSP        ORNE      TASNSP
085200990802     C                   MOVEL     '1'           FINTA4
085300001019     C                   ENDIF
085400061122      * se letto
085500061123     c                   if        finta4 = '0'
085600061122     c                   exsr      recta4
085700061122     c                   endif
085800990802     C*
085900990802     C                   ENDSR
086000061122     C*------------------------------------------------------------------------*
086100061122     C*? RECTA4 - RECUPERO RIFERIMENTO MITTENTE ALFANUMERICO / CODICE FISCALE
086200061122     C*------------------------------------------------------------------------*
086300061122     C     RECTA4        BEGSR
086400061122      * riferimento mittente alfanumerico
086500061122     c                   IF        ta4trc = 'A'
086600061122     c                   movel     ta4not        dta4a
086700061122     c                   movel     §ta4aRMA      Tasrma
086800061122     c                   endif
086900061122      * CODICE FISCALE
087000061122     c                   IF        ta4trc = 'Q'
087100061122     c                   movel     ta4not        dta4q
087200061122      * VERIFICO SE FRANCO O ASSEGNATO
087300061122     c                   If        %subst(tastbl:1:1) = 'F'
087400061122     c                   movel     §TA4QCFM      Tascdf
087500061122     c                   endif
087600061122     c                   If        %subst(tastbl:1:1) = 'A'
087700061122     c                   movel     §TA4QCFD      Tascdf
087800061122     c                   endif
087900061122     c                   endif
088000061122
088100061122     c                   endsr
088200010903     C*------------------------------------------------------------------------*
088300010903     C*? SETTAH - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
088400010903     C*------------------------------------------------------------------------*
088500010903     C     SETTAH        BEGSR
088600010903     C*
088700010903     C* AZZERO CONTATORE RECORD LETTI
088800010903     C                   MOVEL     '0'           FINTAH            1
088900010903     C*
089000010903     C* POSIZIONAMENTO E PRIMA LETTURA
089100010903     C                   Z-ADD     TASAAS        TAHAAS
089200010903     C                   Z-ADD     TASLNP        TAHLNP
089300010903     C                   Z-ADD     TASNRS        TAHNRS
089400010903     C                   Z-ADD     TASNSP        TAHNSP
089500010903     C     KEYTAH        CHAIN     TITAH30C                           99        *FINE-ARCHIV
089600010903     C     *IN99         IFEQ      '1'
089700010903     C                   MOVEL     '1'           FINTAH
089800010903     C                   ENDIF
089900010903     C*
090000010903     C                   ENDSR
090100010903     C*------------------------------------------------------------------------*
090200010903     C*? LETTAH - LETTURA PROSSIMO RECORD TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
090300010903     C*------------------------------------------------------------------------*
090400010903     C     LETTAH        BEGSR
090500010903     C*
090600010903     C                   READ      TITAH30C                               99
090700010903     C     *IN99         IFEQ      '1'                                          *FINE FILE
090800010903     C     TAHAAS        ORNE      TASAAS
090900010903     C     TAHLNP        ORNE      TASLNP
091000010903     C     TAHNRS        ORNE      TASNRS
091100010903     C     TAHNSP        ORNE      TASNSP
091200010903     C                   MOVEL     '1'           FINTAH
091300010903     C                   ENDIF
091400010903     C*
091500010903     C                   ENDSR
091600950307     C*------------------------------------------------------------------------*
091700950307     C*? SETCED - POSIZIONAMENTO E PRIMA LETTURA DEL FILE ECCED
091800950307     C*------------------------------------------------------------------------*
091900950307     C     SETCED        BEGSR
092000950307     C*
092100950307     C* AZZERO CONTATORE RECORD LETTI
092200950307     C                   MOVEL     '0'           FINCED            1
092300950307     C*
092400950307     C* POSIZIONAMENTO E PRIMA LETTURA
092500950307     C                   Z-ADD     TASAAS        CEDAAS
092600950307     C                   Z-ADD     TASLNP        CEDLNP
092700950307     C                   Z-ADD     TASNRS        CEDNRS
092800950307     C                   Z-ADD     TASNSP        CEDNSP
092900950307     C                   MOVEL     TASTBL        CEDTBL
093000001019     C     KEYCED        CHAIN     ECCED30C                           99        *FINE-ARCHIV
093100950307     C     *IN99         IFEQ      '1'
093200950307     C                   MOVEL     '1'           FINCED
093300001019     C                   ENDIF
093400950307     C*
093500950307     C                   ENDSR
093600950307     C*------------------------------------------------------------------------*
093700950307     C*? LETCED - LETTURA PROSSIMO RECORD ECCED
093800950307     C*------------------------------------------------------------------------*
093900950307     C     LETCED        BEGSR
094000950307     C*
094100010124     C                   READ      ECCED30C                               99
094200950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
094300010124     C     CEDAAS        ORNE      TASAAS
094400010124     C     CEDLNP        ORNE      TASLNP
094500010124     C     CEDNRS        ORNE      TASNRS
094600010124     C     CEDNSP        ORNE      TASNSP
094700010124     C     CEDTBL        ORNE      TASTBL
094800950307     C                   MOVEL     '1'           FINCED
094900001019     C                   ENDIF
095000950307     C*
095100950307     C                   ENDSR
095200950307     C*------------------------------------------------------------------------*
095300990802     C*? CHKTAS - CONTROLLA VALIDITA' DEL RECORD DI TITAS
095400950307     C*------------------------------------------------------------------------*
095500950309     C     CHKTAS        BEGSR
095600950307     C*
095700950307     C                   MOVEL     'S'           WRECOK            1
095800000329     C*
095900001023     C* CONTROLLA LO STATO DELLA CONSEGNA IN RELAZIONE AL TIPO BOLLA
096000001023     C* ==> SE TRATTASI DI BOLLA DI RECUPERO (F3/A3/AP) STORICIZZO COMUNQUE
096100001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
096200001018     C     TASDCM        IFEQ      *ZEROS                                       *NON CONSEGNATA
096300001023     C     TASTBL        ANDNE     'F3'
096400001023     C     TASTBL        ANDNE     'A3'
096500001023     C     TASTBL        ANDNE     'AP'
096600010222     C     TASCBO        ANDNE     'E '
096700010222     C     TASCBO        ANDNE     'B '
096800110408     C**                 EXCEPT    ERRDCM
096900000329     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
097000001018     C                   ENDIF
097100001018     C                   ENDIF
097200950309     C*
097300950309     C* CONTROLLA IL CONTRASSEGNO
097400001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
097500950309     C     TASFCA        IFEQ      'S'                                          --- 1 -->
097600950309     C                   Z-ADD     TASAAS        CSBAAS
097700950309     C                   Z-ADD     TASLNP        CSBLNP
097800950309     C                   Z-ADD     TASNRS        CSBNRS
097900950309     C                   Z-ADD     TASNSP        CSBNSP
098000950309     C     KEYCSB        CHAIN     TNCSB03L                           99
098100950309     C     *IN99         IFEQ      '0'                                          --- 2 -->
098200950309     C     CSBSTA        IFNE      9                                            --- 3 -->
098300950309     C     CSBBNA        IFEQ      *ZEROS                                       *NON ANCORA PAGATO
098400950309     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
098500021021     C***                EXCEPT    NOCCSB                                       *STAMPO RECORD
098600021021     C                   ADD       1             CNTCSB                         *CONTEGGIO RECORD
098700950309     C                   ENDIF
098800950309     C                   ENDIF                                                  <-- 3 ---
098900950309     C                   ENDIF                                                  <-- 2 ---
099000950309     C                   ENDIF                                                  <-- 1 ---
099100001018     C                   ENDIF                                                  <-- 1 ---
099200000327     C*
099300000327     C* CONTROLLA I DANNI
099400001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
099500001018     C     TASFDA        IFEQ      'S'
099600040715      **
099700040715      **  Sostituito con il pgm FIDN12R che restituisce l'informazione
099800040715      **   sui danni fra bolla madre e figlia
099900040715      **
100000040715     C**                 Z-ADD     TASAAS        KCTAAS
100100040715     C**                 Z-ADD     TASLNP        KCTLNP
100200040715     C**                 Z-ADD     TASNRS        KCTNRS
100300040715     C**                 Z-ADD     TASNSP        KCTNSP
100400040715     C**   KEYDCT        CHAIN     FNDCT02L                           99
100500040715      *
100600040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
100700040715      * SIA COME MAMMA CHE COME FIGLIA
100800040715     c                   clear                   fidn12ds
100900040715     c                   eval      i12aas = TASAAS
101000040715     c                   eval      i12lnp = TASLNP
101100040715     c                   eval      i12nrs = TASNRS
101200040715     c                   eval      i12nsp = TASNSP
101300040715     c                   eval      i12fel = 'E'
101400040715     c                   call      'FIDN12R'
101500040715     c                   parm                    fidn12ds
101600040715      **
101700040715      * se c'è almeno una C.A. aperta
101800040715      **
101900040715     c     o12nca        ifgt      0
102000040715     C**   *IN99         IFEQ      *OFF                                         *TROVATO DANNO
102100040715      **
102200000327     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
102300021021     C***                EXCEPT    NOCDCT                                       *STAMPO RECORD
102400021021     C                   ADD       1             CNTDCT                         *CONTEGGIO RECORD
102500001018     C                   ENDIF
102600001018     C                   ENDIF
102700001018     C                   ENDIF
102800000404     C*
102900000404     C* CONTROLLA LE GIACENZE
103000001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
103100001018     C     TASFGC        IFEQ      'S'
103200000404     C                   Z-ADD     TASAAS        KCPAAS
103300000404     C                   Z-ADD     TASLNP        KCPLNP
103400000404     C                   Z-ADD     TASNRS        KCPNRS
103500000404     C                   Z-ADD     TASNSP        KCPNSP
103600000404     C                   Z-ADD     0             KCPFRG                         *PRIMO PRG
103700050301     C     KEYGCP        CHAIN(N)  TIGCP51L                           99
103800001018     C     *IN99         IFEQ      *OFF
103900001018     C     GCPFAS        IFLT      50                                           *FASI APERTE
104000000404     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
104100021021     C***                EXCEPT    NOCGCP                                       *STAMPO RECORD
104200021021     C                   ADD       1             CNTGCP                         *CONTEGGIO RECORD
104300001018     C                   ENDIF
104400001018     C                   ENDIF
104500001018     C                   ENDIF
104600001018     C                   ENDIF
104700011227     C*
104800020220     C* SE BOLLA POSTA CANCELLARE SOLO SE + VECCHIA D 2 ANNI
104900011227     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
105000011227     C     TASTSP        IFEQ      'P'                                          *BOLLA POSTA
105100020220     C     TASDSP        ANDGT     DSDATFP
105200011227     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
105300011227     C                   ENDIF
105400011227     C                   ENDIF
105500001019     C*
105600001019     C* CONTEGGIO BOLLE SCARTATE
105700001019     C     WRECOK        IFEQ      'N'
105800001019     C                   ADD       1             NUMNON
105900001019     C                   ENDIF
106000950307     C*
106100950307     C                   ENDSR
106200950309     C*------------------------------------------------------------------------*
106300950309     C*?AGGTAB - AGGIORNA LA TABELLA '$3' CON ULTIMA DATA TRASF. TAS00 > TAS10
106400950309     C*------------------------------------------------------------------------*
106500950309     C     AGGTAB        BEGSR
106600950309     C*
106700950309     C                   OPEN      TABEL00F
106800950309     C*
106900950309     C                   Z-ADD     1             KTBUTE
107000950309     C                   MOVEL     '$3'          KTBCOD
107100950309     C                   MOVEL     '1'           KTBKEY
107200950309     C     KEYTAB        CHAIN     TABEL00F                           99
107300950309     C     *IN99         IFEQ      *ON                                          *NON TROVATA
107400950309     C                   ELSE                                                   *TROVATA
107500950309     C                   MOVEL     TBLUNI        DS$3
107600950309     C                   Z-ADD     PARDTF        §$3DP0                         *ULT.DATA TRASF P>SA
107700020220     C                   Z-ADD     PARDFP        §$3DPP                         *ULT.DATA TRASF P>SA
107800950309     C                   MOVEL     DS$3          TBLUNI
107900950309     C                   UPDATE    TABEL
108000950309     C                   ENDIF
108100950309     C*
108200950309     C                   CLOSE     TABEL00F
108300950309     C*
108400950309     C                   ENDSR
108500950717     C*------------------------------------------------------------------------*
108600950717     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
108700950717     C*------------------------------------------------------------------------*
108800950717     C     SUBDAT        BEGSR
108900950717     C                   TIME                    DTTIME
109000950717     C                   Z-ADD     DTANN         DSANN
109100950717     C                   Z-ADD     DTMES         DSMES
109200950717     C                   Z-ADD     DTGIO         DSGIO
109300950717     C                   Z-ADD     DTSEC         DSSEC
109400950717     C                   ENDSR
109500940912     C*------------------------------------------------------------------------*
109600050705     C*? funzione per scivere file x Laguna
109700940912     C*------------------------------------------------------------------------*
109800050705     C     srlaguna      BEGSR
109900050705     c                   clear                   tnsb34ds
110000050929     c* ricavo il progressivo da impostare nella directory ogni 450000 bolle
110100050720     c* cancellate cambio directory
110200050720     C                   ADD       1             NUMlag                         *REC TRASF TITAS
110300050929     c     numlag        div       450000        sb34prg
110400050720     c                   add       1             sb34prg
110500050705     c                   eval      sb34tla = 'R'
110600050705     c                   eval      sb34aas = tasaas
110700050705     c                   eval      sb34lnp = taslnp
110800050705     c                   eval      sb34nrs = tasnrs
110900050705     c                   eval      sb34nsp = tasnsp
111000050706     c                   eval      sb34dtp = pardtf
111100050706     c                   eval      sb34cop = parcop
111200050705     c                   call      'TNSB34R'
111300050705     c                   parm                    tnsb34ds
111400050705     c                   if        sb34err <> ' '
111500050705     c                   end
111600050705     C                   ENDSR
111700061122     C*------------------------------------------------------------------------*
111800061122     C*? WRTTASSTO - SCRITTURA FILE STORICO TITASS
111900061122     C*------------------------------------------------------------------------*
112000061122     C     WRTTASSTO     BEGSR
112100080312     c* memorizzo directory laguna per sapere dove è stata archiviata la spe
112200080312     c                   clear                   tasdtfp
112300080312     c                   if        §floiml <> ' '
112400080312     c                   move      tasaas        ladaas
112500080312     c                   move      taslnp        ladlnp
112600080312     c                   move      tasnrs        ladnrs
112700080312     c                   move      tasnsp        ladnsp
112800080312     c     klad          chain     tilad01l
112900080312     c                   If        %found(tilad01l)
113000080312     c                   movel     laddir        tasdtfp
113100080312     c                   end
113200080312     c                   end
113300061122      * Decodifica padroncino
113400061122     c                   If        taspdc <> *zeros
113500061122     c     Keyapd        chain     fiapd01l
113600061122     c                   If        %found(fiapd01l)
113700061122     c                   eval      taspdd = apdrsc
113800061122     c                   endif
113900061122     c                   endif
114000061122
114100061122      * recupero c/assegno
114200061122     c                   if        tasfca = 'S'
114300061122     c                   z-add     csbcas        Tascas
114400061122     c                   movel     csbvca        tasvca
114500061122     c                   endif
114600061122
114700061122     c                   move      tasksc        com04
114800061122      * PARTITA IVA / CODICE FISCALE
114900061122     c                   If        %subst(tastbl:1:1) = 'A'  and
115000061122     c                             tascpd <> *blanks
115100061122     c                   eval      tasiva  = tascpd
115200061122     c                   endif
115300061122      * se partita iva non valorizzata
115400061122     c                   If        tasiva = *blanks  or tascdf = *blanks
115500061122      * codificato franco o assegnato
115600061122     c                   if        com04  <>  9999  and com04  <>  8888
115700061122     c                   exsr      reciva
115800061122     c                   if        o69err <> *on
115900061122     c                   if        tascdf = *blanks
116000061122     c                   eval      tasiva = indiva
116100061122     c                   endif
116200061122      * se devo cercare anche il codife fiscale
116300061122     c                   if        tascdf = *blanks
116400061122     c                   eval      tascdf = indcdf
116500061122     c                   endif
116600061122
116700061122     c                   endif
116800061122     c                   endif
116900061122     c                   endif
117000061124      * valorizzo il flag che mi identifica le spedizioni senza segnacollo
117100091229     c                   If        Tasfns <> 'S' and tasncd = *zeros
117200061124     c                   eval      tassss = 'S'
117300061124     c                   endif
117400061122
117500061122     c                   write     titassto                             99
117600061122     C   99              EXCEPT    ERRsto
117700061122     C   99              MOVEL     '1'           FINL00                         *FINE PGM
117800061122     C  N99              ADD       1             NUMsto                         *REC TRASF FNEVB
117900061122
118000061122     C                   ENDSR
118100061122     C*
118200050705     C*------------------------------------------------------------------------*
118300050705     C*? RUTINZ - ROUTINE INIZIALE
118400050705     C*------------------------------------------------------------------------*
118500050705     C     RUTINZ        BEGSR
118600931115     C*
118700931115     C* PARAMETRI IN INPUT AL PGM
118800931115     C     *ENTRY        PLIST
118900931115     C                   PARM                    KPJBA
119000931115     C                   MOVEL     KPJBU         PARAM
119100950309     C*
119200950309     C* DEFINISCO CAMPI
119300950309     C     *LIKE         DEFINE    TBLKUT        KTBUTE
119400950309     C     *LIKE         DEFINE    TBLCOD        KTBCOD
119500950309     C     *LIKE         DEFINE    TBLKEY        KTBKEY
119600970903     C     *LIKE         DEFINE    CETNBC        MINNBC
119700040715     C***  *LIKE         DEFINE    DCTAAS        KCTAAS                         *FNDCT00F
119800040715     C***  *LIKE         DEFINE    DCTLNP        KCTLNP
119900040715     C***  *LIKE         DEFINE    DCTNRS        KCTNRS
120000040715     C***  *LIKE         DEFINE    DCTNSP        KCTNSP
120100050301     C     *LIKE         DEFINE    GCPAAS        KCPAAS                         *TIGCP00F
120200000404     C     *LIKE         DEFINE    GCPLNP        KCPLNP
120300000404     C     *LIKE         DEFINE    GCPNRS        KCPNRS
120400000404     C     *LIKE         DEFINE    GCPNSP        KCPNSP
120500000404     C     *LIKE         DEFINE    GCPFRG        KCPFRG
120600050301     C     *LIKE         DEFINE    GAPAAS        KAPAAS                         *TIGAP00F
120700000404     C     *LIKE         DEFINE    GAPLNP        KAPLNP
120800000404     C     *LIKE         DEFINE    GAPNRS        KAPNRS
120900000404     C     *LIKE         DEFINE    GAPNSP        KAPNSP
121000050301     C     *LIKE         DEFINE    GNPAGC        KNPAGC                         *TIGNP00F
121100000404     C     *LIKE         DEFINE    GNPFGC        KNPFGC
121200000404     C     *LIKE         DEFINE    GNPNGC        KNPNGC
121300950309     C*
121400950309     C* CHIAVE LETTURA TABEL00F - COMPLETA
121500950309     C     KEYTAB        KLIST
121600950309     C                   KFLD                    KTBUTE
121700950309     C                   KFLD                    KTBCOD
121800950309     C                   KFLD                    KTBKEY
121900080925     C     KEYTAB1       KLIST
122000080925     C                   KFLD                    KTBUTE
122100080925     C                   KFLD                    KTBCOD
122200080925     C                   Z-ADD     1             KTBUTE
122300080925     C                   MOVEL     'TB'          KTBCOD
122400950309     C*
122500950309     C* CHIAVE LETTURA TNCSB01L - COMPLETA
122600950309     C     KEYCSB        KLIST
122700950309     C                   KFLD                    CSBAAS
122800950309     C                   KFLD                    CSBLNP
122900950309     C                   KFLD                    CSBNRS
123000950309     C                   KFLD                    CSBNSP
123100000327     C*
123200000327     C* CHIAVE LETTURA FNDCT02L - COMPLETA
123300040715     C***  KEYDCT        KLIST
123400040715     C***                KFLD                    KCTAAS
123500040715     C***                KFLD                    KCTLNP
123600040715     C***                KFLD                    KCTNRS
123700040715     C***                KFLD                    KCTNSP
123800930208     C*
123900990802     C* CHIAVE LETTURA TITAS38C - PARZIALE
124000930621     C     KEYTAS        KLIST
124100941123     C                   KFLD                    TASAAS
124200941123     C                   KFLD                    TASMGS
124300950307     C*
124400990802     C* CHIAVE LETTURA TITA730C - COMPLETA
124500950307     C     KEYTA7        KLIST
124600950307     C                   KFLD                    TA7AAS
124700950307     C                   KFLD                    TA7LNP
124800950307     C                   KFLD                    TA7NRS
124900950307     C                   KFLD                    TA7NSP
125000950307     C                   KFLD                    TA7TBL
125100990802     C*
125200990802     C* CHIAVE LETTURA TITAA30C - COMPLETA
125300990802     C     KEYTAA        KLIST
125400990802     C                   KFLD                    TAAAAS
125500990802     C                   KFLD                    TAALNP
125600990802     C                   KFLD                    TAANRS
125700990802     C                   KFLD                    TAANSP
125800990802     C*
125900990802     C* CHIAVE LETTURA TITAT30C - COMPLETA
126000990802     C     KEYTAT        KLIST
126100990802     C                   KFLD                    TATAAS
126200990802     C                   KFLD                    TATLNP
126300990802     C                   KFLD                    TATNRS
126400990802     C                   KFLD                    TATNSP
126500990802     C*
126600990802     C* CHIAVE LETTURA TITA430C - COMPLETA
126700990802     C     KEYTA4        KLIST
126800990802     C                   KFLD                    TA4AAS
126900990802     C                   KFLD                    TA4LNP
127000990802     C                   KFLD                    TA4NRS
127100990802     C                   KFLD                    TA4NSP
127200010903     C*
127300010903     C* CHIAVE LETTURA TITAH30C - COMPLETA
127400010903     C     KEYTAH        KLIST
127500010903     C                   KFLD                    TAHAAS
127600010903     C                   KFLD                    TAHLNP
127700010903     C                   KFLD                    TAHNRS
127800010903     C                   KFLD                    TAHNSP
127900950209     C*
128000950307     C* CHIAVE LETTURA ECCET30C - COMPLETA
128100950220     C     KEYCET        KLIST
128200950220     C                   KFLD                    CETAAS                         ANNO SPEDIZIONE
128300950220     C                   KFLD                    CETLNP                         LINEA DI PARTENZA
128400950220     C                   KFLD                    CETNRS                         NUMERO SERIE
128500950220     C                   KFLD                    CETNSP                         NUMERO SPEDIZIONE
128600950220     C                   KFLD                    CETTBL                         TIPO BOLLA
128700950307     C*
128800950307     C* CHIAVE LETTURA ECCED30C - COMPLETA
128900950307     C     KEYCED        KLIST
129000950307     C                   KFLD                    CEDAAS                         ANNO SPEDIZIONE
129100950307     C                   KFLD                    CEDLNP                         LINEA DI PARTENZA
129200950307     C                   KFLD                    CEDNRS                         NUMERO SERIE
129300950307     C                   KFLD                    CEDNSP                         NUMERO SPEDIZIONE
129400950307     C                   KFLD                    CEDTBL                         TIPO BOLLA
129500980325     C*
129600980325     C* CHIAVE LETTURA ECFIM30C - COMPLETA
129700980325     C     KEYFIM        KLIST
129800980325     C                   KFLD                    FIMAAS                         ANNO SPEDIZIONE
129900980325     C                   KFLD                    FIMLNP                         LINEA DI PARTENZA
130000980325     C                   KFLD                    FIMNRS                         NUMERO SERIE
130100980325     C                   KFLD                    FIMNSP                         NUMERO SPEDIZIONE
130200980325     C                   KFLD                    FIMTBL                         TIPO BOLLA
130300000327     C*
130400000327     C* CHIAVE LETTURA FNEVB01L - COMPLETA
130500000327     C     KEYEVB        KLIST
130600000327     C                   KFLD                    EVBAAS                         ANNO SPEDIZIONE
130700000327     C                   KFLD                    EVBLNP                         LINEA DI PARTENZA
130800000327     C                   KFLD                    EVBNRS                         NUMERO SERIE
130900000327     C                   KFLD                    EVBNSP                         NUMERO SPEDIZIONE
131000000404     C*
131100050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
131200000404     C     KEYGCP        KLIST
131300000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
131400000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
131500000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
131600000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
131700000404     C                   KFLD                    KCPFRG                         PRG RIAPERTURE
131800000404     C*
131900050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
132000000404     C     KE2GCP        KLIST
132100000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
132200000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
132300000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
132400000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
132500000404     C*
132600050301     C* CHIAVE LETTURA TIGAP01L - PARZIALE
132700000404     C     KEYGAP        KLIST
132800000404     C                   KFLD                    KAPAAS                         ANNO SPEDIZIONE
132900000404     C                   KFLD                    KAPLNP                         LINEA DI PARTENZA
133000000404     C                   KFLD                    KAPNRS                         NUMERO SERIE
133100000404     C                   KFLD                    KAPNSP                         NUMERO SPEDIZIONE
133200000404     C*
133300050301     C* CHIAVE LETTURA TIGNP01L - PARZIALE
133400000404     C     KEYGNP        KLIST
133500000404     C                   KFLD                    KNPAGC                         ANNO APERTURA
133600000404     C                   KFLD                    KNPFGC                         FILILAE APERTURA
133700000404     C                   KFLD                    KNPNGC                         NUMERO GIACENZA
133800040303     C*
133900040531     C* CHIAVE LETTURA FIAR531C
134000040303     C     KEYAR5        KLIST
134100040303     C                   KFLD                    AR5AAS
134200040303     C                   KFLD                    AR5LNP
134300040303     C                   KFLD                    AR5NRS
134400040303     C                   KFLD                    AR5NSP
134500060919     C*
134600060919     C* CHIAVE LETTURA TITAI01L
134700060919     C     KEYtai        KLIST
134800060919     C                   KFLD                    taiAAS
134900060919     C                   KFLD                    taiLNP
135000060919     C                   KFLD                    taiNRS
135100060919     C                   KFLD                    taiNSP
135200060919     C                   KFLD                    taiTBL
135300081103
135400081103     C* CHIAVE LETTURA TITAI01L
135500081103     C     KEYrab        KLIST
135600081103     C                   KFLD                    rabAAS
135700081103     C                   KFLD                    rabLNP
135800081103     C                   KFLD                    rabNRS
135900081103     C                   KFLD                    rabNSP
136000061122
136100061122     C* CHIAVE LETTURA Fiapd01l
136200061122     C     KEYapd        KLIST
136300061122     C                   KFLD                    trcapd
136400061122     C                   KFLD                    taspdc
136500040823      *
136600040823     C     kbol          klist
136700040823     C                   kfld                    tasaas
136800040823     C                   kfld                    taslnp
136900040823     C                   kfld                    tasnrs
137000040823     C                   kfld                    tasnsp
137100080312      *
137200080312     C     klad          klist
137300080312     C                   kfld                    LADAAS
137400080312     C                   kfld                    LADLNP
137500080312     C                   kfld                    LADNRS
137600080312     C                   kfld                    LADNSP
137700000327     C*
137800000327     C* CALCOLA LA DATA CORRENTE
137900000327     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
138000000327     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
138100000327     C                   Z-ADD     N8            G08DAT
138200000327     C                   Z-ADD     *ZEROS        G08INV
138300000327     C                   MOVEL     '0'           G08ERR
138400000327     C                   CALL      'XSRDA8'
138500000327     C                   PARM                    WLBDA8
138600000327     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
138700930208     C*
138800930208     C* IMPOSTO A ZERO IL FINE PGM
138900930208     C                   MOVEL     '0'           FINL00            1
139000931105     C                   MOVEL     *BLANKS       RSUT             20
139100040922     C                   Z-ADD     *ZEROS        NUMblp            9 0
139200040922     C                   Z-ADD     *ZEROS        NUMarb            9 0
139300950308     C                   Z-ADD     *ZEROS        NUMTAS            9 0
139400050720     C                   Z-ADD     *ZEROS        NUMlag            9 0
139500950308     C                   Z-ADD     *ZEROS        NUMTA7            9 0
139600990802     C                   Z-ADD     *ZEROS        NUMTAA            9 0
139700990802     C                   Z-ADD     *ZEROS        NUMTAT            9 0
139800990802     C                   Z-ADD     *ZEROS        NUMTA4            9 0
139900010903     C                   Z-ADD     *ZEROS        NUMTAH            9 0
140000950307     C                   Z-ADD     *ZEROS        NUMCET            9 0
140100950308     C                   Z-ADD     *ZEROS        NUMCED            9 0
140200980325     C                   Z-ADD     *ZEROS        NUMFIM            9 0
140300970904     C                   Z-ADD     *ZEROS        NUMECT            9 0
140400970904     C                   Z-ADD     *ZEROS        NUMECD            9 0
140500000327     C                   Z-ADD     *ZEROS        NUMEVB            9 0
140600000404     C                   Z-ADD     *ZEROS        NUMGCP            9 0
140700000404     C                   Z-ADD     *ZEROS        NUMGAP            9 0
140800000404     C                   Z-ADD     *ZEROS        NUMGNP            9 0
140900001019     C                   Z-ADD     *ZEROS        NUMNON            9 0
141000040303     C                   Z-ADD     *ZEROS        NUMAR5            9 0
141100060919     C                   Z-ADD     *ZEROS        NUMtai            9 0
141200081103     C                   Z-ADD     *ZEROS        NUMrab            9 0
141300061122     C                   Z-ADD     *ZEROS        NUMsto            9 0
141400021021     C                   Z-ADD     *ZEROS        CNTGCP            9 0
141500021021     C                   Z-ADD     *ZEROS        CNTDCT            9 0
141600021021     C                   Z-ADD     *ZEROS        CNTCSB            9 0
141700040823      * Apro il Fnblp di filiale
141800040823     c                   Clear                   comman
141900040823     c                   Movel(p)  cmd(1)        comman
142000040823     c                   Eval      %Subst(comman:13:8) = 'FNBLP01L'
142100040910      *test sistema informativo di prova
142200040910     c                   if        %subst(knsif:7:1) = 'P'
142300040910     c                   Eval      %Subst(comman:30:10) = wlibp
142400040910     c                   else
142500040910     c                   Eval      %Subst(comman:30:10) = wlib
142600040910     c                   endif
142700040910
142800040823     c                   Eval      comman =
142900040823     c                             %trim(comman) + '/FNBLP01L)'
143000040823     c                   Call      'QCMDEXC'                            99
143100040823     c                   Parm                    comman
143200040823     c                   Parm                    lenght
143300040823     c                   If        *In99
143400040823     c                   seton                                        lr
143500040823     c                   Else
143600040823     c                   Open      Fnblp01l
143700040823     c                   EndIf
143800040823      *
143900040823      * Apro  Fnarb di filiale
144000040823     c                   Clear                   comman
144100040823     c                   Movel(p)  cmd(1)        comman
144200040823     c                   Eval      %Subst(comman:13:8) = 'FNARB01L'
144300040910      *test sistema informativo di prova
144400040910     c                   if        %subst(knsif:7:1) = 'P'
144500040910     c                   Eval      %Subst(comman:30:10) = wlibp
144600040910     c                   else
144700040910     c                   Eval      %Subst(comman:30:10) = wlib
144800040910     c                   endif
144900040910
145000040823     c                   Eval      comman =
145100040823     c                             %trim(comman) + '/FNARB01L)'
145200040823     c                   Call      'QCMDEXC'                            99
145300040823     c                   Parm                    comman
145400040823     c                   Parm                    lenght
145500040823     c                   If        *In99
145600040823     c                   seton                                        lr
145700040823     c                   Else
145800040823     c                   Open      Fnarb01l
145900040823     c                   EndIf
146000040823      *
146100931115     C*
146200950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
146300080925     c* carico i tipi bolla RECUPERO per evitare di salvarli nel TITASS0F
146400081103     c                   clear                   conta
146500080926     c                   open      tabel00f
146600080925     c     keytab1       setll     tabel00f
146700080925     c                   do        *hival
146800080925     c     keytab1       reade(n)  tabel00f
146900080925     c                   if        %eof(tabel00f)
147000080925     c                   leave
147100080925     c                   end
147200080925     c                   movel(p)  tbluni        dstb
147300080925     c                   if        §TBRBL ='R'
147400080925     c                   add       1             conta             2 0
147500080925     c                   movel     tblkey        tbl(conta)
147600080925     c                   end
147700080925     c                   enddo
147800080926     c                   close     tabel00f
147900000000     C*
148000000000     C                   ENDSR
148100940912     O*------------------------------------------------------------------------*
148200940905     OPRTF198   E            TESTA            02
148300950309     O                                            8 'TNSB38R '
148400931105     O                       RSUT                45
148500990802     O                                           82 'TRASF. TITASP -> SALVAT.'
148600931115     O                                           90 'DAL:'
148700950309     O                       PARDTI              99
148800931115     O                                          110 'AL:'
148900950309     O                       PARDTF             119
149000020220     O                                          128 '/PT'
149100020220     O                       PARDFP             132
149200931105     O                                          150 'Data:'
149300950717     O                       DSDAT6             159 '  /  /  '
149400931105     O                                          167 'Ora:'
149500950717     O                       DTORA6             178 '  .  .  '
149600931105     O                                          190 'Pagina:'
149700931105     O                       PAGE          Z    198
149800931105     O          E            TESTA       1  1
149900931105     O                       TES(2)              66
150000931105     O                       TES(3)             132
150100931105     O                       TES(4)             198
150200931105     O          E            TESTA          1
150300931105     O                       TES(1)              66
150400931105     O                       TES(1)             132
150500931105     O                       TES(1)             198
150600001019     O*------------------------------------
150700001019     O          E            ERRDCM         1
150800001019     O                       TASAAS            +  5
150900001019     O                       TASLNP            +  1
151000001019     O                       TASNRS            +  1
151100001019     O                       TASNSP            +  1
151200001019     O                       TASTBL            +  1
151300001019     O                                         +  1 'BOLLA NON CONS.    '
151400931105     O*------------------------------------
151500950307     O          E            ERRTAS         1
151600950307     O                       TASAAS            +  5
151700950307     O                       TASLNP            +  1
151800950307     O                       TASNRS            +  1
151900950307     O                       TASNSP            +  1
152000950307     O                       TASTBL            +  1
152100990802     O                                         +  1 'ERR. WRITE TITAS   '
152200950307     O*------------------------------------
152300950307     O          E            ERRTA7         1
152400950307     O                       TA7AAS            +  5
152500950307     O                       TA7LNP            +  1
152600950307     O                       TA7NRS            +  1
152700950307     O                       TA7NSP            +  1
152800950307     O                       TA7TBL            +  1
152900950307     O                       TA7SVN            +  1
153000990802     O                                         +  1 'ERR. WRITE TITA7   '
153100990802     O*------------------------------------
153200990802     O          E            ERRTAA         1
153300990802     O                       TAAAAS            +  5
153400990802     O                       TAALNP            +  1
153500990802     O                       TAANRS            +  1
153600990802     O                       TAANSP            +  1
153700990802     O                       TAATRC            +  1
153800990802     O                                         +  1 'ERR. WRITE TITAA   '
153900990802     O*------------------------------------
154000990802     O          E            ERRTAT         1
154100990802     O                       TATAAS            +  5
154200990802     O                       TATLNP            +  1
154300990802     O                       TATNRS            +  1
154400990802     O                       TATNSP            +  1
154500990802     O                       TATNSC            +  1
154600990802     O                                         +  1 'ERR. WRITE TITAT   '
154700990802     O*------------------------------------
154800990802     O          E            ERRTA4         1
154900990802     O                       TA4AAS            +  5
155000990802     O                       TA4LNP            +  1
155100990802     O                       TA4NRS            +  1
155200990802     O                       TA4NSP            +  1
155300990802     O                       TA4TRC            +  1
155400990802     O                                         +  1 'ERR. WRITE TITA4   '
155500010903     O*------------------------------------
155600010903     O          E            ERRTAH         1
155700010903     O                       TAHAAS            +  5
155800010903     O                       TAHLNP            +  1
155900010903     O                       TAHNRS            +  1
156000010903     O                       TAHNSP            +  1
156100010903     O                       TAHTRC            +  1
156200010903     O                                         +  1 'ERR. WRITE TITAH   '
156300950307     O*------------------------------------
156400950307     O          E            ERRCET         1
156500950307     O                       CETAAS            +  5
156600950307     O                       CETLNP            +  1
156700950307     O                       CETNRS            +  1
156800950307     O                       CETNSP            +  1
156900950307     O                       CETTBL            +  1
157000950309     O                                         +  1 'ERR. WRITE ECCET   '
157100950307     O*------------------------------------
157200950307     O          E            ERRCED         1
157300950307     O                       CEDAAS            +  5
157400950307     O                       CEDLNP            +  1
157500950307     O                       CEDNRS            +  1
157600950307     O                       CEDNSP            +  1
157700950307     O                       CEDTBL            +  1
157800950307     O                       CEDCMP            +  1
157900950309     O                                         +  1 'ERR. WRITE ECCED   '
158000980325     O*------------------------------------
158100980325     O          E            ERRFIM         1
158200980325     O                       FIMAAS            +  5
158300980325     O                       FIMLNP            +  1
158400980325     O                       FIMNRS            +  1
158500980325     O                       FIMNSP            +  1
158600980325     O                       FIMTBL            +  1
158700980325     O                                         +  1 'ERR. WRITE ECFIM   '
158800970903     O*------------------------------------
158900970903     O          E            ERRECT         1
159000970903     O                       ECTNBC        Z   +  5
159100970903     O                       ECTCMP        Z   +  1
159200970903     O                       ECTDDC            +  1 '    /  /  '
159300970903     O                                         +  1 'ERR. WRITE ECECT   '
159400970903     O*------------------------------------
159500970903     O          E            ERRECD         1
159600970903     O                       ECDNBC        Z   +  5
159700970903     O                       ECDCMP        Z   +  1
159800970903     O                       ECDFRA            +  1
159900970903     O                       ECDIOE            +  1
160000970903     O                       ECDKCC        Z   +  1
160100970903     O                       ECDKSC        Z   +  1
160200970903     O                       ECDRIC            +  1
160300970903     O                       ECDCCF        Z   +  1
160400970903     O                       ECDDDO            +  1 '    /  /  '
160500970903     O                                         +  1 'ERR. WRITE ECECD   '
160600000404     O*------------------------------------
160700000404     O          E            ERREVB         1
160800000404     O                       EVBAAS            +  5
160900000404     O                       EVBLNP            +  1
161000000404     O                       EVBNRS            +  1
161100000404     O                       EVBNSP            +  1
161200000404     O                                         +  1 'ERR. WRITE FNEVB   '
161300000404     O*------------------------------------
161400000404     O          E            ERRGCP         1
161500000404     O                       GCPAAS            +  5
161600000404     O                       GCPLNP            +  1
161700000404     O                       GCPNRS            +  1
161800000404     O                       GCPNSP            +  1
161900050301     O                                         +  1 'ERR. WRITE TIGCP   '
162000050303     O          E            ERRGCP1        1
162100050303     O                       GCPAAS            +  5
162200050303     O                       GCPLNP            +  1
162300050303     O                       GCPNRS            +  1
162400050303     O                       GCPNSP            +  1
162500050303     O                                         +  1 'GIACENZA ANCORA IN FILIALE'
162600000404     O*------------------------------------
162700000404     O          E            ERRGAP         1
162800000404     O                       GAPAAS            +  5
162900000404     O                       GAPLNP            +  1
163000000404     O                       GAPNRS            +  1
163100000404     O                       GAPNSP            +  1
163200050301     O                                         +  1 'ERR. WRITE TIGAP   '
163300000404     O*------------------------------------
163400000404     O          E            ERRGNP         1
163500000404     O                       GNPAGC            +  5
163600000404     O                       GNPFGC            +  4
163700000404     O                       GNPNGC            +  1
163800050301     O                                         +  1 'ERR. WRITE TIGNP   '
163900040303     O*------------------------------------
164000040303     O          E            ERRAR5         1
164100040303     O                       AR5AAS            +  5
164200040303     O                       AR5LNP            +  1
164300040303     O                       AR5NRS            +  1
164400040303     O                       AR5NSP            +  1
164500040303     O                                         +  1 'ERR. WRITE FIAR5   '
164600060919     O          E            ERRtai         1
164700060919     O                       taiAAS            +  5
164800060919     O                       taiLNP            +  1
164900060919     O                       taiNRS            +  1
165000060919     O                       taiNSP            +  1
165100060919     O                       taitbl            +  1
165200060919     O                                         +  1 'ERR. WRITE TITAI   '
165300081103     O          E            ERRrab         1
165400081103     O                       rabAAS            +  5
165500081103     O                       rabLNP            +  1
165600081103     O                       rabNRS            +  1
165700081103     O                       rabNSP            +  1
165800081103     O                                         +  1 'ERR. WRITE FIRAB   '
165900950309     O*------------------------------------
166000000404     O          E            NOCCSB         1
166100950309     O                       CSBAAS            +  5
166200950309     O                       CSBLNP            +  1
166300950309     O                       CSBNRS            +  1
166400950309     O                       CSBNSP            +  1
166500000404     O                                         +  1 'BOLLA NON CANC:TNCSB'
166600000404     O*------------------------------------
166700040715     O*****     E            NOCDCT         1
166800040715     O*****                  DCTAAS            +  5
166900040715     O*****                  DCTLNP            +  1
167000040715     O*****                  DCTNRS            +  1
167100040715     O*****                  DCTNSP            +  1
167200040715     O*****                                    +  1 'BOLLA NON CANC:FNDCT'
167300040715     O          E            NOCDCT         1
167400040715     O                       i12AAS            +  5
167500040715     O                       i12LNP            +  1
167600040715     O                       i12NRS            +  1
167700040715     O                       i12NSP            +  1
167800040715     O                                         +  1 'BOLLA NON CANC:FNDCT'
167900000404     O*------------------------------------
168000000404     O          E            NOCGCP         1
168100000404     O                       GCPAAS            +  5
168200000404     O                       GCPLNP            +  1
168300000404     O                       GCPNRS            +  1
168400000404     O                       GCPNSP            +  1
168500050301     O                                         +  1 'BOLLA NON CANC:TIGCP'
168600040922     O*------------------------------------
168700040922     O          E            ERRblp         1
168800040922     O                       blpAAS            +  5
168900040922     O                       blpLNP            +  1
169000040922     O                       blpNRS            +  1
169100040922     O                       blpNSP            +  1
169200040922     O                                         +  1 'ERR. Trovato reK su FNBLP'
169300040922     O*------------------------------------
169400040922     O          E            ERRarb         1
169500040922     O                       arbAAS            +  5
169600040922     O                       arbLNP            +  1
169700040922     O                       arbNRS            +  1
169800040922     O                       arbNSP            +  1
169900040922     O                                         +  1 'ERR. Trovato reK su FNARB'
170000061122     O          E            ERRSTO         1
170100061122     O                       TASAAS            +  5
170200061122     O                       TASLNP            +  1
170300061122     O                       TASNRS            +  1
170400061122     O                       TASNSP            +  1
170500061122     O                       TASTBL            +  1
170600061122     O                                         +  1 'ERR. WRITE STORICO '
170700040922     O*------------------------------------
170800950308     O          E            STAFIN         1
170900990802     O                                           30 'N.REC.TRASF. TITAS'
171000950308     O                       NUMTAS        2     44
171100950308     O          E            STAFIN         1
171200990802     O                                           30 'N.REC.TRASF. TITA7'
171300950308     O                       NUMTA7        2     44
171400990802     O          E            STAFIN         1
171500990802     O                                           30 'N.REC.TRASF. TITAA'
171600990802     O                       NUMTAA        2     44
171700990802     O          E            STAFIN         1
171800990802     O                                           30 'N.REC.TRASF. TITAT'
171900990802     O                       NUMTAT        2     44
172000990802     O          E            STAFIN         1
172100990802     O                                           30 'N.REC.TRASF. TITA4'
172200990802     O                       NUMTA4        2     44
172300010903     O          E            STAFIN         1
172400010903     O                                           30 'N.REC.TRASF. TITAH'
172500010903     O                       NUMTAH        2     44
172600950308     O          E            STAFIN         1
172700950308     O                                           30 'N.REC.TRASF. ECCET'
172800950308     O                       NUMCET        2     44
172900950308     O          E            STAFIN         1
173000950308     O                                           30 'N.REC.TRASF. ECCED'
173100950308     O                       NUMCED        2     44
173200980325     O          E            STAFIN         1
173300980325     O                                           30 'N.REC.TRASF. ECFIM'
173400980325     O                       NUMFIM        2     44
173500970903     O          E            STAFIN         1
173600970903     O                                           30 'N.REC.TRASF. ECECT'
173700970903     O                       NUMECT        2     44
173800970903     O          E            STAFIN         1
173900970903     O                                           30 'N.REC.TRASF. ECECD'
174000970903     O                       NUMECD        2     44
174100000327     O          E            STAFIN         1
174200000327     O                                           30 'N.REC.TRASF. FNEVB'
174300000327     O                       NUMEVB        2     44
174400000404     O          E            STAFIN         1
174500050301     O                                           30 'N.REC.TRASF. TIGCP'
174600000404     O                       NUMGCP        2     44
174700000404     O          E            STAFIN         1
174800050301     O                                           30 'N.REC.TRASF. TIGAP'
174900000404     O                       NUMGAP        2     44
175000000404     O          E            STAFIN         1
175100050301     O                                           30 'N.REC.TRASF. TIGNP'
175200000404     O                       NUMGNP        2     44
175300040310     O          E            STAFIN         1
175400040310     O                                           30 'N.REC. BOLLE FIAR5'
175500040310     O                       NUMAR5        2     44
175600060919     O          E            STAFIN         1
175700060919     O                                           30 'N.REC. BOLLE TITAI'
175800060919     O                       NUMTAI        2     44
175900081103     O          E            STAFIN         1
176000081103     O                                           30 'N.REC. BOLLE FIRAB'
176100081103     O                       NUMRAB        2     44
176200061122     O          E            STAFIN         1
176300061122     O                                           30 'N.BOLLE IN STORICO'
176400061122     O                       NUMSTO        2     44
176500001019     O          E            STAFIN         1
176600001020     O                                           30 'N.REC. BOLLE SCART'
176700001019     O                       NUMNON        2     44
176800021021     O          E            STAFIN         1
176900021021     O                                           30 'DI CUI:'
177000021021     O          E            STAFIN         1
177100021021     O                                           30 'X GIACENZA APERTA '
177200021021     O                       CNTGCP        2     44
177300021021     O          E            STAFIN         1
177400021021     O                                           30 'X DANNO PENDENTE  '
177500021021     O                       CNTDCT        2     44
177600021021     O          E            STAFIN         1
177700021021     O                                           30 'X C/A ATTIVO      '
177800021021     O                       CNTCSB        2     44
177900040922     O          E            STAFIN         1
178000040922     O                                           30 'N.REK.Trovati FNBLP'
178100040922     O                       NUMblp        2     44
178200040922     O          E            STAFIN         1
178300040922     O                                           30 'N.REK.Trovati FNARB'
178400040922     O                       NUMarb        2     44
178500060626     O          E            STAFIN         1
178600060626     O                                           +0 'Chiusuara pgm: FINTAS'
178700060626     O                       fintas              +1
178800060626     O                       tasdsp              +1
178900060626     O                       dsdatf              +1
179000060626     O               98                          +1 'SHTDN'
179100060626     O                                           +1 'FINL00'
179200060626     O                       finl00              +1
179300950308     O          E            STAFIN           60
179400931115     O                                           76 '*** FINE STAMPA ***'
179500931115     O                                          150 'Data:'
179600950717     O                       DSDAT6             159 '  /  /  '
179700990802     O                                          167 'Ora:'
179800950717     O                       DTORA6             178 '  :  :  '
179900940912     O*------------------------------------------------------------------------*
180000970903     O* AGGIORNAMENTO FILES
180100970903     O*------------------------------------------------------------------------*
180200970903     OECECT     E            AGGECT
180300970903     O                       ECTATB
180400970903     O                       ECTFTR
180500970903     OECECD     E            AGGECD
180600970903     O                       ECDATB
180700970903     O                       ECDFTR
180800970903     O*------------------------------------------------------------------------*
180900040823** cmd
181000040823OVRDBF FILE(        ) TOFILE(
181100931105**  TES - RIGHE IN STAMPA
181200931105------------------------------------------------------------------  1
181300931105 DATA    ORA       AA LNP SE N.SPED. TB     ERR  DESCRIZIONE ERROR  2
181400950308E                                                                   3
181500931105                                                                    4
181600931105                                                                    5
181700931105                                                                    6
181800931105                                                                    7
181900931105                                                                    8
182000931105                                                                    9
182100931105                                                                   10
182200931105                                                                   11
182300931105                                                                   12
182400931105                                                                   13
182500931105                                                                   14
182600931105                                                                   15
