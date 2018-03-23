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
008001131029     FFNEVB22L  UF   E           K DISK
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
024300120709
024400120709      * RCD DOC FIAR5
024500120709     d dAR5doc       e ds
024600120709
024700940912     C*------------------------------------------------------------------------*
024800940912     C*?MAIN LINE
024900940912     C*------------------------------------------------------------------------*
025000000000     C*
025100930129     C* ROUTINE INIZIALE
025200930129     C                   EXSR      RUTINZ
025300931105     C*
025400931105     C* STAMPA RIGA DI TESTATA
025500931105     C                   EXCEPT    TESTA
025600930601     C*
025700930601     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
025800930601     C     FINL00        IFEQ      '0'                                          --- 1 -->
025900000000     C*
026000990802     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA SU TITAS
026100950307     C                   EXSR      SETTAS
026200930128     C*
026300990802     C* CICLO FINO A FINE FILE TITAS OPPURE
026400950308     C* RICHIESTA CHIUSURA PGM OPPURE
026500950308     C* FINE PGM PER ERRORE
026600950308     C                   SETOFF                                       98
026700950307     C     FINTAS        DOWEQ     '0'                                          --- 2 -->
026800950308     C     *IN98         ANDEQ     '0'
026900950308     C     FINL00        ANDEQ     '0'
027000080214      * verifico   che non esistano ARB e BLP altrimenti non cancello
027100040823      * e leggo un'altro rekord
027200040823     C     Kbol          Chain     fnblp01l                           33
027300040922     C  N33              ADD       1             NUMblp                         *REC TRASF TITA7
027400040922     C  n33              EXCEPT    ERRblp
027500080214     C     kbol          Chain     fnarb01l                           34
027600080214     C  N34              ADD       1             NUMarb                         *REC TRASF TITA7
027700080214     C  n34              EXCEPT    ERRarb
027800080214     c                   if        not *in33 or not *in34
027900080214     c                   goto      altrorek
028000080214     c                   end
028100950307     C*
028200001019     C                   WRITE     TITASS00                             99      *SCRIVO TITASS
028300001019     C  N99              DELETE    TITASP00                             99      *CANCELLO TITASP
028400950307     C   99              EXCEPT    ERRTAS
028500950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
028600990802     C  N99              ADD       1             NUMTAS                         *REC TRASF TITAS
028700050705     c* se bolla ha immagine di laguna scrivo record sul file TILAD00F
028800050705     c                   if        not *in99
028900050705     c                   movel     tasflo        dtasflo
029000050705     c                   if        §floiml <> ' '
029100050705     c                   exsr      srlaguna
029200050705     c                   end
029300120709       //?se la bolla ha le immagini dei documenti firmati dal cliente da
029400120709       //?tornare al mittente scrivo nuovo record sul file TIJDD00F
029500120709      /free
029600120709        AR5trd = 'DOC';
029700120709        chain (TASaas:TASlnp:TASnrs:TASnsp:AR5trd) FIAR531C;
029800120709        IF  %found(FIAR531C);
029900120709          dAR5doc = AR5uni;
030000120709          IF  §AR5jfid <> *blanks;
030100120709            exsr srJdoc;
030200120709          ENDIF;
030300120709        ENDIF;
030400120709      /end-free
030500050705     c                   end
030600950307     C*
030700990802     C* SE ESISTE TITA7 TRASFERISCO ANCHE QUELLO
030800950307     C     TASSV3        IFNE      *BLANKS                                      --- 3 -->
030900990802     C                   EXSR      SETTA7                                       *PRIMO TITA7
031000950307     C     FINTA7        DOWEQ     '0'                                          --- 4 -->
031100990802     C                   WRITE     TITA7S00                             99      *SCRIVO TITA7S
031200001019     C  N99              DELETE    TITA730C                             99      *CANCELLO TITA730C
031300950307     C   99              EXCEPT    ERRTA7
031400950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
031500990802     C  N99              ADD       1             NUMTA7                         *REC TRASF TITA7
031600990802     C                   EXSR      LETTA7                                       *SUCCESSIVO TITA7
031700950307     C                   ENDDO                                                  <-- 4 ---
031800950307     C                   ENDIF                                                  <-- 3 ---
031900990802     C*
032000990802     C* TRASFERISCE TITAA - ANAGRAFICHE
032100990802     C                   EXSR      SETTAA                                       *PRIMO TITAA
032200990802DO  3C     FINTAA        DOWEQ     '0'
032300990802     C                   WRITE     TITAAS00                             99      *SCRIVO TITAAS
032400001019     C  N99              DELETE    TITAA30C                             99      *CANCELLO TITAA30C
032500990802     C   99              EXCEPT    ERRTAA
032600990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
032700990802     C  N99              ADD       1             NUMTAA                         *REC TRASF TITAA
032800990802     C                   EXSR      LETTAA                                       *SUCCESSIVO TITAA
032900990802E   3C                   ENDDO
033000061121     C* verifico se scritto i dati anagrafici del mittente altrimenti aggancio anagrafica
033100061121     c                   if        tasrsm = *blanks
033200061121     c                   exsr      recana
033300061121     c                   endif
033400990802     C*
033500990802     C* TRASFERISCE TITAT - SEGNACOLLI
033600990802     C                   EXSR      SETTAT                                       *PRIMO TITAT
033700990802DO  3C     FINTAT        DOWEQ     '0'
033800990802     C                   WRITE     TITATS00                             99      *SCRIVO TITATS
033900001019     C  N99              DELETE    TITAT30C                             99      *CANCELLO TITAT30C
034000990802     C   99              EXCEPT    ERRTAT
034100990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
034200990802     C  N99              ADD       1             NUMTAT                         *REC TRASF TITAT
034300990802     C                   EXSR      LETTAT                                       *SUCCESSIVO TITAT
034400990802E   3C                   ENDDO
034500990802     C*
034600990802     C* TRASFERISCE TITA4 - RIFERIMENTI
034700990802     C                   EXSR      SETTA4                                       *PRIMO TITA4
034800990802DO  3C     FINTA4        DOWEQ     '0'
034900990802     C                   WRITE     TITA4S00                             99      *SCRIVO TITA4S
035000001019     C  N99              DELETE    TITA430C                             99      *CANCELLO TITA430C
035100990802     C   99              EXCEPT    ERRTA4
035200990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
035300990802     C  N99              ADD       1             NUMTA4                         *REC TRASF TITA4
035400990802     C                   EXSR      LETTA4                                       *SUCCESSIVO TITA4
035500990802E   3C                   ENDDO
035600010903     C*
035700010903     C* TRASFERISCE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
035800010903     C                   EXSR      SETTAH                                       *PRIMO TITAH
035900010903DO  3C     FINTAH        DOWEQ     '0'
036000010903     C                   WRITE     TITAHS00                             99      *SCRIVO TITAHS
036100010903     C  N99              DELETE    TITAH30C                             99      *CANCELLO TITAH30C
036200010903     C   99              EXCEPT    ERRTAH
036300010903     C   99              MOVEL     '1'           FINL00                         *FINE PGM
036400010903     C  N99              ADD       1             NUMTAH                         *REC TRASF TITAH
036500010903     C                   EXSR      LETTAH                                       *SUCCESSIVO TITAH
036600010903E   3C                   ENDDO
036700950307     C*
036800950307     C* SE ESISTE ECCET LO TRASFERISCE
036900971008     C                   Z-ADD     999           MINNBC                         MIMIMO BLOCCO
037000950307     C                   Z-ADD     TASAAS        CETAAS                         ANNO SPEDIZIONE
037100950307     C                   Z-ADD     TASLNP        CETLNP                         LINEA DI PARTENZA
037200950307     C                   Z-ADD     TASNRS        CETNRS                         NUMERO SERIE
037300950307     C                   Z-ADD     TASNSP        CETNSP                         NUMERO SPEDIZIONE
037400950307     C                   MOVEL     TASTBL        CETTBL                         TIPO BOLLA
037500950307     C     KEYCET        CHAIN     ECCET30C                           99
037600950307     C     *IN99         IFEQ      *OFF                                         --- 3 --> *TROVATO
037700970903     C*
037800970903     C* MEMORIZZA IL NUMERO BLOCCO COMPETENZE TESTATA PIU' PICCOLO
037900971008IF  4C     CETNBC        IFGT      *ZEROS
038000971008IF  5C     CETNBC        IFLT      MINNBC
038100970903     C                   Z-ADD     CETNBC        MINNBC
038200971008E   5C                   ENDIF
038300971008E   4C                   ENDIF
038400970903     C*
038500950309     C                   WRITE     ECCETS                               99      *SCRIVO ECCETS
038600001019     C  N99              DELETE    ECCET30C                             99      *CANCELLO ECCET30C
038700950307     C   99              EXCEPT    ERRCET
038800950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
038900950308     C  N99              ADD       1             NUMCET                         *REC TRASF ECCET
039000950307     C*
039100950307     C* SE ESISTE ECCED TRASFERISCO ANCHE QUELLO
039200950307     C     CETDET        IFEQ      'S'                                          --- 4 -->
039300950307     C                   EXSR      SETCED                                       *PRIMO ECCED
039400950307     C     FINCED        DOWEQ     '0'                                          --- 5 -->
039500950309     C                   WRITE     ECCEDS                               99      *SCRIVO ECCEDS
039600001020     C  N99              DELETE    ECCED30C                             99      *CANCELLO ECCED30C
039700950307     C   99              EXCEPT    ERRCED
039800950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
039900950308     C  N99              ADD       1             NUMCED                         *REC TRASF ECCED
040000950307     C                   EXSR      LETCED                                       *SUCCESSIVO ECCED
040100950307     C                   ENDDO                                                  <-- 5 ---
040200950307     C                   ENDIF                                                  <-- 4 ---
040300950307     C                   ENDIF                                                  <-- 3 ---
040400980325     C*
040500980325     C* SE ESISTE ECFIM LO TRASFERISCE
040600980325     C                   Z-ADD     TASAAS        FIMAAS                         ANNO SPEDIZIONE
040700980325     C                   Z-ADD     TASLNP        FIMLNP                         LINEA DI PARTENZA
040800980325     C                   Z-ADD     TASNRS        FIMNRS                         NUMERO SERIE
040900980325     C                   Z-ADD     TASNSP        FIMNSP                         NUMERO SPEDIZIONE
041000980325     C                   MOVEL     TASTBL        FIMTBL                         TIPO BOLLA
041100001019     C     KEYFIM        CHAIN     ECFIM30C                           99
041200980325     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
041300980325     C                   WRITE     ECFIMS00                             99      *SCRIVE
041400001019     C  N99              DELETE    ECFIM30C                             99      *CANCELLA
041500980325     C   99              EXCEPT    ERRFIM
041600980325     C   99              MOVEL     '1'           FINL00                         *FINE PGM
041700980325     C  N99              ADD       1             NUMFIM                         *REC TRASF ECCET
041800011127     C     KEYFIM        READE     ECFIM30C                               99
041900980325     C                   ENDDO                                                  <-- 3 ---
042000000327     C*
042100000327     C* SE ESISTE FNEVB LO TRASFERISCE
042200000327     C                   Z-ADD     TASAAS        EVBAAS                         ANNO SPEDIZIONE
042300000327     C                   Z-ADD     TASLNP        EVBLNP                         LINEA DI PARTENZA
042400000327     C                   Z-ADD     TASNRS        EVBNRS                         NUMERO SERIE
042500000327     C                   Z-ADD     TASNSP        EVBNSP                         NUMERO SPEDIZIONE
042600001019     C     KEYEVB        CHAIN     FNEVB01L                           99
042700000327     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
042800000327     C                   WRITE     FNEVBS                               99      *SCRIVE
042900001019     C  N99              DELETE    FNEVB01L                             99      *CANCELLA
042901131029     c                   if        not *in99
042902131029     C     Kevb2f        chain     FNEVB22L
042903131029     c                   if        %found(fnevb22l)
042904131029     c                   delete    fnevb22l
042905131029     c                   endif
042906131029     c                   endif
043000000327     C   99              EXCEPT    ERREVB
043100000327     C   99              MOVEL     '1'           FINL00                         *FINE PGM
043200000327     C  N99              ADD       1             NUMEVB                         *REC TRASF FNEVB
043300000327     C     KEYEVB        READE     FNEVB01L                               99
043400000327     C                   ENDDO                                                  <-- 3 ---
043500000404     C*
043600050301     C* SE ESISTE TIGCP LO TRASFERISCE
043700001018     C     TASFGC        IFEQ      'S'
043800000404     C                   Z-ADD     TASAAS        KCPAAS                         ANNO SPEDIZIONE
043900000404     C                   Z-ADD     TASLNP        KCPLNP                         LINEA DI PARTENZA
044000000404     C                   Z-ADD     TASNRS        KCPNRS                         NUMERO SERIE
044100000404     C                   Z-ADD     TASNSP        KCPNSP                         NUMERO SPEDIZIONE
044200050301     C     KE2GCP        CHAIN     TIGCP51L                           99
044300001018     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
044400050303     c     gcpsede       ifeq      'S'
044500050301     C                   WRITE     TIGCPS                               99      *SCRIVE
044600050301     C  N99              DELETE    TIGCP51L                             99      *CANCELLA
044700000404     C   99              EXCEPT    ERRGCP
044800000404     C   99              MOVEL     '1'           FINL00                         *FINE PGM
044900000404     C  N99              ADD       1             NUMGCP                         *REC TRASF FNEVB
045000000404     C*
045100050301     C* SE ESISTE TIGAP LO TRASFERISCE
045200000404     C                   Z-ADD     GCPAAS        KAPAAS                         ANNO SPEDIZIONE
045300000404     C                   Z-ADD     GCPLNP        KAPLNP                         LINEA DI PARTENZA
045400000404     C                   Z-ADD     GCPNRS        KAPNRS                         NUMERO SERIE
045500000404     C                   Z-ADD     GCPNSP        KAPNSP                         NUMERO SPEDIZIONE
045600050301     C     KEYGAP        CHAIN     TIGAP01L                           99
045700001018     C     *IN99         DOWEQ     *OFF
045800050301     C                   WRITE     TIGAPS                               99      *SCRIVE
045900050301     C  N99              DELETE    TIGAP01L                             99      *CANCELLA
046000001017     C   99              EXCEPT    ERRGAP
046100001017     C   99              MOVEL     '1'           FINL00                         *FINE PGM
046200001017     C  N99              ADD       1             NUMGAP                         *REC TRASF FNEVB
046300050301     C     KEYGAP        READE     TIGAP01L                               99
046400001018     C                   ENDDO
046500000404     C*
046600050301     C* SE ESISTE TIGNP LO TRASFERISCE
046700000404     C                   Z-ADD     GCPAGC        KNPAGC                         ANNO APERTURA
046800000404     C                   Z-ADD     GCPFGC        KNPFGC                         FILIALE APERTURA
046900000404     C                   Z-ADD     GCPNGC        KNPNGC                         NUMERO GIACENZA
047000050301     C     KEYGNP        CHAIN     TIGNP01L                           99
047100001018     C     *IN99         DOWEQ     *OFF
047200050301     C                   WRITE     TIGNPS                               99      *SCRIVE
047300050301     C  N99              DELETE    TIGNP01L                             99      *CANCELLA
047400001017     C   99              EXCEPT    ERRGNP
047500001017     C   99              MOVEL     '1'           FINL00                         *FINE PGM
047600001017     C  N99              ADD       1             NUMGNP                         *REC TRASF FNEVB
047700050301     C     KEYGNP        READE     TIGNP01L                               99
047800001018     C                   ENDDO
047900050303     c                   else
048000050303     C                   EXCEPT    ERRGCP1
048100060707     C*                  MOVEL     '1'           FINL00                         *FINE PGM
048200050303     C                   ENDIF
048300000404     C*
048400000404     C* LETTURA SUCCESSIVA GIACENZE
048500050301     C     KE2GCP        READE     TIGCP51L                               99
048600001018     C                   ENDDO
048700001018     C                   ENDIF
048800040303     C*
048900040303     C* SE ESISTE FIAR5 LO TRASFERISCE
049000040303     C                   Z-ADD     TASAAS        AR5AAS                         ANNO SPEDIZIONE
049100040303     C                   Z-ADD     TASLNP        AR5LNP                         LINEA DI PARTENZA
049200040303     C                   Z-ADD     TASNRS        AR5NRS                         NUMERO SERIE
049300040303     C                   Z-ADD     TASNSP        AR5NSP                         NUMERO SPEDIZIONE
049400040531     C     KEYAR5        CHAIN     FIAR531C                           99
049500040303     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
049600040303     C                   WRITE     FIAR5S                               99      *SCRIVE
049700040531     C  N99              DELETE    FIAR531C                             99      *CANCELLA
049800040303     C   99              EXCEPT    ERRAR5
049900040303     C   99              MOVEL     '1'           FINL00                         *FINE PGM
050000040303     C  N99              ADD       1             NUMAR5                         *REC TRASF FNEVB
050100040531     C     KEYAR5        READE     FIAR531C                               99
050200040303     C                   ENDDO                                                  <-- 3 ---
050300060919     C*
050400060919     C* SE ESISTE TITAI LO TRASFERISCE
050500060919     C                   Z-ADD     TASAAS        taiAAS                         ANNO SPEDIZIONE
050600060919     C                   Z-ADD     TASLNP        taiLNP                         LINEA DI PARTENZA
050700060919     C                   Z-ADD     TASNRS        taiNRS                         NUMERO SERIE
050800060919     C                   Z-ADD     TASNSP        taiNSP                         NUMERO SPEDIZIONE
050900060919     C                   movel     TASTBL        taiTBL                         NUMERO SPEDIZIONE
051000060919     C     KEYtai        CHAIN     titai01l                           99
051100060919     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
051200060919     C                   WRITE     titaiS                               99      *SCRIVE
051300060919     C  N99              DELETE    titai01l                             99      *CANCELLA
051400060919     C   99              EXCEPT    ERRtai
051500060919     C   99              MOVEL     '1'           FINL00                         *FINE PGM
051600060919     C  N99              ADD       1             NUMtai                         *REC TRASF FNEVB
051700060919     C     KEYtai        READE     titai01l                               99
051800060919     C                   ENDDO                                                  <-- 3 ---
051900081103     C* SE ESISTE FIRAB LO TRASFERISCE
052000081103     C                   Z-ADD     TASAAS        rabAAS                         ANNO SPEDIZIONE
052100081103     C                   Z-ADD     TASLNP        rabLNP                         LINEA DI PARTENZA
052200081103     C                   Z-ADD     TASNRS        rabNRS                         NUMERO SERIE
052300081103     C                   Z-ADD     TASNSP        rabNSP                         NUMERO SPEDIZIONE
052400081103     C     KEYrab        CHAIN     firab01l                           99
052500081103     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
052600081103     C                   WRITE     firabS                               99      *SCRIVE
052700081103     C  N99              DELETE    firab01l                             99      *CANCELLA
052800081103     C   99              EXCEPT    ERRrab
052900081103     C   99              MOVEL     '1'           FINL00                         *FINE PGM
053000081103     C  N99              ADD       1             NUMrab                         *REC TRASF FNEVB
053100081103     C     KEYrab        READE     firab01l                               99
053200081103     C                   ENDDO                                                  <-- 3 ---
053300061214      * decodifico alcuni dati per il nuovo storico  e lo scrivo se non bolla Poste
053400061214     c                   If        tastsp <> 'P'
053500080926     c* se bolla di recupero non scrivo storico
053600080926     c                   z-add     1             conta
053700080926     c     tastbl        lookup    tbl(conta)                             44
053800080926     c                   if        not *in44
053900061122     c                   EXSR      wrttassto
054000061214     c                   endif
054100080926     c                   endif
054200061122
054300000000     C*
054400980325     C* LETTURA SUCCESSIVA BOLLE
054500040823     c     altrorek      tag
054600040823
054700060707     c                   if        finl00 = '0'
054800990802     C                   EXSR      LETTAS                                       *SUCCESSIVO TITAS
054900060707     c                   end
055000950308     C                   SHTDN                                        98        *CONTR.CHIUSURA PGM
055100930128     C*
055200950307     C                   ENDDO                                                  <-- 2 ---
055300950307     C                   ENDIF                                                  <-- 1 ---
055400970903     C*
055500970903     C* PULISCE I CODICI COMPETENZA -TESTATA E DETTAGLIO- CON NUMERO BLOCCO
055600970903     C* MINORE DI QUELLO PIU' PICCOLO CON CUI SONO STATE CALCOLATE LE BOLLE
055700971008IF  1C     MINNBC        IFNE      999
055800970903     C     *LOVAL        SETLL     ECECT01L
055900970903     C                   READ      ECECT01L                               97
056000971008DO  2C     *IN97         DOWEQ     *OFF
056100971008IF  3C     ECTATB        IFEQ      *BLANKS
056200971008IF  4C     ECTNBC        IFLT      MINNBC
056300970903     C                   MOVEL     'A'           ECTATB
056400970903     C                   MOVEL     *BLANKS       ECTFTR
056500970903     C                   EXCEPT    AGGECT                                       *AGGIORNAMENTO CAMPI
056600970903     C                   WRITE     ECECTS                               97      *SCRIVO ECECTS
056700970903     C   97              EXCEPT    ERRECT                                       *SCRIVE ERRORE
056800970903     C   97              MOVEL     '1'           FINL00                         *FINE PGM
056900970903     C  N97              ADD       1             NUMECT                         *REC TRASF ECECT
057000971008E   4C                   ENDIF
057100971008E   3C                   ENDIF
057200970903     C                   READ      ECECT01L                               97
057300971008E   2C                   ENDDO
057400950309     C*
057500970903     C     *LOVAL        SETLL     ECECD01L
057600970903     C                   READ      ECECD01L                               97
057700971008DO  2C     *IN97         DOWEQ     *OFF
057800971008IF  3C     ECDATB        IFEQ      *BLANKS
057900971008IF  4C     ECDNBC        IFLT      MINNBC
058000970903     C                   MOVEL     'A'           ECDATB
058100970903     C                   MOVEL     *BLANKS       ECDFTR
058200970903     C                   EXCEPT    AGGECD                                       *AGGIORNAMENTO CAMPI
058300970903     C                   WRITE     ECECDS                               97      *SCRIVO ECECDS
058400970903     C   97              EXCEPT    ERRECD                                       *SCRIVE ERRORE
058500970903     C   97              MOVEL     '1'           FINL00                         *FINE PGM
058600970903     C  N97              ADD       1             NUMECD                         *REC TRASF ECECD
058700971008E   4C                   ENDIF
058800971008E   3C                   ENDIF
058900970903     C                   READ      ECECD01L                               97
059000971008E   2C                   ENDDO
059100971008E   1C                   ENDIF
059200970903     C*
059300950309     C* AGGIORNA LA TABELLA CON L'ULTIMA DATA DI TRASFERIM. (SOLO SE TUTTO OK)
059400950309     C     *IN98         IFEQ      '0'
059500950309     C     FINL00        ANDEQ     '0'
059600950309     C                   EXSR      AGGTAB
059700950309     C                   ENDIF
059800931105     C*
059900931105     C* STAMPA RIGA DI FINE STAMPA
060000950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
060100931105     C                   EXCEPT    STAFIN
060200050705     c* chiudo il pgm di archiviazione Laguna
060300050705     C                   CLEAR                   Tnsb34ds
060400050705     C                   MOVEL     'C'           sb34tla
060500050705     C                   CALL      'TNSB34R'
060600050705     C                   PARM                    TNSB34ds
060700000000     C*
060800000000     C                   SETON                                        LR
060900940912     C*------------------------------------------------------------------------*
061000990802     C*? SETTAS - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAS
061100940912     C*------------------------------------------------------------------------*
061200950307     C     SETTAS        BEGSR
061300931115     C*
061400931115     C* AZZERO CONTATORE RECORD LETTI
061500950307     C                   MOVEL     '0'           FINTAS            1
061600930208     C*
061700930316     C* POSIZIONAMENTO E PRIMA LETTURA
061800941123     C                   Z-ADD     PARDTI        DSDATI
061900941123     C                   Z-ADD     PARDTF        DSDATF
062000020220     C                   Z-ADD     PARDFP        DSDATFP
062100941123     C                   Z-ADD     DSAASI        TASAAS
062200941123     C                   Z-ADD     DSMGSI        TASMGS
062300990802     C     KEYTAS        SETLL     TITAS38C                           99        *FINE-ARCHIV
062400930625     C*
062500930625     C     *IN99         IFEQ      '1'
062600950307     C                   MOVEL     '1'           FINTAS
062700930625     C                   ELSE
062800950307     C                   EXSR      LETTAS
062900001020     C                   ENDIF
063000930208     C*
063100930208     C                   ENDSR
063200940912     C*------------------------------------------------------------------------*
063300990802     C*? LETTAS - LETTURA PROSSIMO RECORD TITAS
063400940912     C*------------------------------------------------------------------------*
063500950307     C     LETTAS        BEGSR
063600930625     C*
063700930625     C* LEGGO FINO A:
063800930625     C*     - FINE FILE O SUPERATO LIMITE FINALE
063900930625     C*     - TROVATO RECORD VALIDO
064000930625     C                   MOVEL     'N'           WRECOK
064100950307     C     FINTAS        DOWEQ     '0'                                          --- 1 -->
064200930625     C     WRECOK        ANDEQ     'N'
064300061121      * pulisco lo storico
064400061121     c                   clear                   titassto
064500930208     C*
064600930316     C* LETTURE SUCCESSIVE ALLA PRIMA
064700990802     C                   READ      TITAS38C                               99
064800960413     C*
064900960413     C     *IN99         IFEQ      '1'                                          *FINE FILE
065000020220     C     TASDSP        ORGT      DSDATF                                       *SUPERATA DATA FINE
065100950307     C                   MOVEL     '1'           FINTAS
065200930625     C                   ELSE
065300950309     C                   EXSR      CHKTAS                                       *CONTR.VALIDITA REC.
065400001019     C                   ENDIF
065500930625     C*
065600001019     C                   ENDDO                                                  <-- 1 ---
065700930208     C*
065800930208     C                   ENDSR
065900950307     C*------------------------------------------------------------------------*
066000990802     C*? SETTA7 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA7
066100950307     C*------------------------------------------------------------------------*
066200950307     C     SETTA7        BEGSR
066300950307     C*
066400950307     C* AZZERO CONTATORE RECORD LETTI
066500950307     C                   MOVEL     '0'           FINTA7            1
066600950307     C*
066700950307     C* POSIZIONAMENTO E PRIMA LETTURA
066800950307     C                   Z-ADD     TASAAS        TA7AAS
066900950307     C                   Z-ADD     TASLNP        TA7LNP
067000950307     C                   Z-ADD     TASNRS        TA7NRS
067100950307     C                   Z-ADD     TASNSP        TA7NSP
067200950307     C                   MOVEL     TASTBL        TA7TBL
067300001019     C*
067400001019     C     KEYTA7        CHAIN     TITA730C                           99        *FINE-ARCHIV
067500950307     C     *IN99         IFEQ      '1'
067600950307     C                   MOVEL     '1'           FINTA7
067700001019     C                   ENDIF
067800950307     C*
067900950307     C                   ENDSR
068000950307     C*------------------------------------------------------------------------*
068100990802     C*? LETTA7 - LETTURA PROSSIMO RECORD TITA7
068200950307     C*------------------------------------------------------------------------*
068300001019     C     LETTA7        BEGSR
068400001020     C*
068500001020     C* LETTURE SUCCESSIVE ALLA PRIMA
068600010124     C                   READ      TITA730C                               99
068700950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
068800010124     C     TA7AAS        ORNE      TASAAS
068900010124     C     TA7LNP        ORNE      TASLNP
069000010124     C     TA7NRS        ORNE      TASNRS
069100010124     C     TA7NSP        ORNE      TASNSP
069200010124     C     TA7TBL        ORNE      TASTBL
069300950307     C                   MOVEL     '1'           FINTA7
069400001019     C                   ENDIF
069500950307     C*
069600950307     C                   ENDSR
069700990802     C*------------------------------------------------------------------------*
069800990802     C*? SETTAA - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAA - ANAGRAFICHE
069900990802     C*------------------------------------------------------------------------*
070000990802     C     SETTAA        BEGSR
070100990802     C*
070200990802     C* AZZERO CONTATORE RECORD LETTI
070300990802     C                   MOVEL     '0'           FINTAA            1
070400990802     C*
070500990802     C* POSIZIONAMENTO E PRIMA LETTURA
070600990802     C                   Z-ADD     TASAAS        TAAAAS
070700990802     C                   Z-ADD     TASLNP        TAALNP
070800990802     C                   Z-ADD     TASNRS        TAANRS
070900990802     C                   Z-ADD     TASNSP        TAANSP
071000001019     C     KEYTAA        CHAIN     TITAA30C                           99        *FINE-ARCHIV
071100990802     C     *IN99         IFEQ      '1'
071200990802     C                   MOVEL     '1'           FINTAA
071300061121      * recupero dati anagrafici del mittente
071400061121     c                   else
071500061121     c                   exsr      rectaa
071600001019     C                   ENDIF
071700990802     C*
071800990802     C                   ENDSR
071900990802     C*------------------------------------------------------------------------*
072000990802     C*? LETTAA - LETTURA PROSSIMO RECORD TITAA - ANAGRAFICHE
072100990802     C*------------------------------------------------------------------------*
072200990802     C     LETTAA        BEGSR
072300010124     C*
072400010124     C                   READ      TITAA30C                               99
072500990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
072600010130     C     TAAAAS        ORNE      TASAAS
072700010130     C     TAALNP        ORNE      TASLNP
072800010130     C     TAANRS        ORNE      TASNRS
072900010130     C     TAANSP        ORNE      TASNSP
073000990802     C                   MOVEL     '1'           FINTAA
073100001019     C                   ENDIF
073200061121      * se letto
073300061123     c                   if        fintaa = '0'
073400061121     c                   exsr      rectaa
073500061121     c                   endif
073600990802     C*
073700990802     C                   ENDSR
073800061121     C*------------------------------------------------------------------------*
073900061121     C*? RECTAA - RECUPERO DATI ANAGRAFICI MITTENTE
074000061121     C*------------------------------------------------------------------------*
074100061121     C     RECTAA        BEGSR
074200061121      * dati anagrafici mittente
074300061121     c                   IF        taatrc = 'M'
074400061121     c                   eval      tasrsm = taarsc
074500061121     c                   eval      tasinm = taaind
074600061121     c                   eval      tascam = taacap
074700061121     c                   eval      taslom = taaloc
074800061121     c                   eval      tasprm = taaprv
074900061121     c                   eval      tasnzm = taanaz
075000061122      * iva  per i franchi non coidficati
075100061122     c                   if        %subst(tastbl:1:1) = 'F'
075200061122     c                   eval      tasiva = taacpi
075300061122     c                   endif
075400061122
075500061121     c                   endif
075600061121
075700061121     c                   endsr
075800061121     C*------------------------------------------------------------------------*
075900061121     C*? RECANA - RECUPERO DA ANAGRAFICO CLIENTE
076000061121     C*------------------------------------------------------------------------*
076100061121     C     RECANA        BEGSR
076200061121     C                   clear                   ds_CNACO
076300061121     C                   clear                   ds_CNIND
076400061121     C                   clear                   ds_CNCLP
076500061121     C                   clear                   ds_FNCLS
076600061121     c                   z-add     TASccm        I69kac
076700061121     c                   z-add     TASccm        I69kin
076800061121     c                   call      'TIBS69R'
076900061121     c                   parm                    TIBS69ds
077000061121     c                   parm                    ds_CNACO
077100061121     c                   parm                    ds_CNIND
077200061121     c                   parm                    ds_CNCLP
077300061121     c                   parm                    ds_FNCLS
077400061122if  3c                   if        O69err <> *on
077500061122     c                   movel     ACOrag        TASrsm
077600061122     c                   movel     INDvia        TASinm
077700061122     c                   movel     INDcap        TAScam
077800061122     c                   movel     INDcit        TASlom
077900061122     c                   movel     INDprv        TASprm
078000061122     c                   movel     INDsta        TASnzm
078100061122e   3c                   endif
078200061121
078300061121     c                   endsr
078400061122     C*------------------------------------------------------------------------*
078500061122     C*? RECIVA - RECUPERO DA ANAGRAFICO CLIENTE IVA E/O CODICE FISCALE
078600061122     C*------------------------------------------------------------------------*
078700061122     C     RECIVA        BEGSR
078800061122     C                   clear                   ds_CNACO
078900061122     C                   clear                   ds_CNIND
079000061122     C                   clear                   ds_CNCLP
079100061122     C                   clear                   ds_FNCLS
079200061122     c                   z-add     TASKSC        I69kin
079300061122     c                   call      'TIBS69R'
079400061122     c                   parm                    TIBS69ds
079500061122     c                   parm                    ds_CNACO
079600061122     c                   parm                    ds_CNIND
079700061122     c                   parm                    ds_CNCLP
079800061122     c                   parm                    ds_FNCLS
079900061122
080000061122     c                   endsr
080100990802     C*------------------------------------------------------------------------*
080200990802     C*? SETTAT - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAT - SEGNACOLLI
080300990802     C*------------------------------------------------------------------------*
080400990802     C     SETTAT        BEGSR
080500990802     C*
080600990802     C* AZZERO CONTATORE RECORD LETTI
080700990802     C                   MOVEL     '0'           FINTAT            1
080800990802     C*
080900990802     C* POSIZIONAMENTO E PRIMA LETTURA
081000990802     C                   Z-ADD     TASAAS        TATAAS
081100990802     C                   Z-ADD     TASLNP        TATLNP
081200990802     C                   Z-ADD     TASNRS        TATNRS
081300990802     C                   Z-ADD     TASNSP        TATNSP
081400001019     C     KEYTAT        CHAIN     TITAT30C                           99        *FINE-ARCHIV
081500990802     C     *IN99         IFEQ      '1'
081600990802     C                   MOVEL     '1'           FINTAT
081700001019     C                   ENDIF
081800990802     C*
081900990802     C                   ENDSR
082000990802     C*------------------------------------------------------------------------*
082100990802     C*? LETTAT - LETTURA PROSSIMO RECORD TITAT - SEGNACOLLI
082200990802     C*------------------------------------------------------------------------*
082300990802     C     LETTAT        BEGSR
082400010124     C*
082500010124     C                   READ      TITAT30C                               99
082600990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
082700010124     C     TATAAS        ORNE      TASAAS
082800010124     C     TATLNP        ORNE      TASLNP
082900010124     C     TATNRS        ORNE      TASNRS
083000010124     C     TATNSP        ORNE      TASNSP
083100990802     C                   MOVEL     '1'           FINTAT
083200001019     C                   ENDIF
083300990802     C*
083400990802     C                   ENDSR
083500990802     C*------------------------------------------------------------------------*
083600990802     C*? SETTA4 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA4 - RIFERIMENTI
083700990802     C*------------------------------------------------------------------------*
083800990802     C     SETTA4        BEGSR
083900990802     C*
084000990802     C* AZZERO CONTATORE RECORD LETTI
084100990802     C                   MOVEL     '0'           FINTA4            1
084200990802     C*
084300990802     C* POSIZIONAMENTO E PRIMA LETTURA
084400990802     C                   Z-ADD     TASAAS        TA4AAS
084500990802     C                   Z-ADD     TASLNP        TA4LNP
084600990802     C                   Z-ADD     TASNRS        TA4NRS
084700990802     C                   Z-ADD     TASNSP        TA4NSP
084800001019     C     KEYTA4        CHAIN     TITA430C                           99        *FINE-ARCHIV
084900990802     C     *IN99         IFEQ      '1'
085000990802     C                   MOVEL     '1'           FINTA4
085100061122      * recupero riferimento mittente alfanumerico
085200061122     c                   else
085300061122     c                   exsr      recta4
085400001019     C                   ENDIF
085500990802     C*
085600990802     C                   ENDSR
085700990802     C*------------------------------------------------------------------------*
085800990802     C*? LETTA4 - LETTURA PROSSIMO RECORD TITA4 - RIFERIMENTI
085900990802     C*------------------------------------------------------------------------*
086000990802     C     LETTA4        BEGSR
086100010124     C*
086200010124     C                   READ      TITA430C                               99
086300990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
086400010124     C     TA4AAS        ORNE      TASAAS
086500010124     C     TA4LNP        ORNE      TASLNP
086600010124     C     TA4NRS        ORNE      TASNRS
086700010124     C     TA4NSP        ORNE      TASNSP
086800990802     C                   MOVEL     '1'           FINTA4
086900001019     C                   ENDIF
087000061122      * se letto
087100061123     c                   if        finta4 = '0'
087200061122     c                   exsr      recta4
087300061122     c                   endif
087400990802     C*
087500990802     C                   ENDSR
087600061122     C*------------------------------------------------------------------------*
087700061122     C*? RECTA4 - RECUPERO RIFERIMENTO MITTENTE ALFANUMERICO / CODICE FISCALE
087800061122     C*------------------------------------------------------------------------*
087900061122     C     RECTA4        BEGSR
088000061122      * riferimento mittente alfanumerico
088100061122     c                   IF        ta4trc = 'A'
088200061122     c                   movel     ta4not        dta4a
088300061122     c                   movel     §ta4aRMA      Tasrma
088400061122     c                   endif
088500061122      * CODICE FISCALE
088600061122     c                   IF        ta4trc = 'Q'
088700061122     c                   movel     ta4not        dta4q
088800061122      * VERIFICO SE FRANCO O ASSEGNATO
088900061122     c                   If        %subst(tastbl:1:1) = 'F'
089000061122     c                   movel     §TA4QCFM      Tascdf
089100061122     c                   endif
089200061122     c                   If        %subst(tastbl:1:1) = 'A'
089300061122     c                   movel     §TA4QCFD      Tascdf
089400061122     c                   endif
089500061122     c                   endif
089600061122
089700061122     c                   endsr
089800010903     C*------------------------------------------------------------------------*
089900010903     C*? SETTAH - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
090000010903     C*------------------------------------------------------------------------*
090100010903     C     SETTAH        BEGSR
090200010903     C*
090300010903     C* AZZERO CONTATORE RECORD LETTI
090400010903     C                   MOVEL     '0'           FINTAH            1
090500010903     C*
090600010903     C* POSIZIONAMENTO E PRIMA LETTURA
090700010903     C                   Z-ADD     TASAAS        TAHAAS
090800010903     C                   Z-ADD     TASLNP        TAHLNP
090900010903     C                   Z-ADD     TASNRS        TAHNRS
091000010903     C                   Z-ADD     TASNSP        TAHNSP
091100010903     C     KEYTAH        CHAIN     TITAH30C                           99        *FINE-ARCHIV
091200010903     C     *IN99         IFEQ      '1'
091300010903     C                   MOVEL     '1'           FINTAH
091400010903     C                   ENDIF
091500010903     C*
091600010903     C                   ENDSR
091700010903     C*------------------------------------------------------------------------*
091800010903     C*? LETTAH - LETTURA PROSSIMO RECORD TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
091900010903     C*------------------------------------------------------------------------*
092000010903     C     LETTAH        BEGSR
092100010903     C*
092200010903     C                   READ      TITAH30C                               99
092300010903     C     *IN99         IFEQ      '1'                                          *FINE FILE
092400010903     C     TAHAAS        ORNE      TASAAS
092500010903     C     TAHLNP        ORNE      TASLNP
092600010903     C     TAHNRS        ORNE      TASNRS
092700010903     C     TAHNSP        ORNE      TASNSP
092800010903     C                   MOVEL     '1'           FINTAH
092900010903     C                   ENDIF
093000010903     C*
093100010903     C                   ENDSR
093200950307     C*------------------------------------------------------------------------*
093300950307     C*? SETCED - POSIZIONAMENTO E PRIMA LETTURA DEL FILE ECCED
093400950307     C*------------------------------------------------------------------------*
093500950307     C     SETCED        BEGSR
093600950307     C*
093700950307     C* AZZERO CONTATORE RECORD LETTI
093800950307     C                   MOVEL     '0'           FINCED            1
093900950307     C*
094000950307     C* POSIZIONAMENTO E PRIMA LETTURA
094100950307     C                   Z-ADD     TASAAS        CEDAAS
094200950307     C                   Z-ADD     TASLNP        CEDLNP
094300950307     C                   Z-ADD     TASNRS        CEDNRS
094400950307     C                   Z-ADD     TASNSP        CEDNSP
094500950307     C                   MOVEL     TASTBL        CEDTBL
094600001019     C     KEYCED        CHAIN     ECCED30C                           99        *FINE-ARCHIV
094700950307     C     *IN99         IFEQ      '1'
094800950307     C                   MOVEL     '1'           FINCED
094900001019     C                   ENDIF
095000950307     C*
095100950307     C                   ENDSR
095200950307     C*------------------------------------------------------------------------*
095300950307     C*? LETCED - LETTURA PROSSIMO RECORD ECCED
095400950307     C*------------------------------------------------------------------------*
095500950307     C     LETCED        BEGSR
095600950307     C*
095700010124     C                   READ      ECCED30C                               99
095800950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
095900010124     C     CEDAAS        ORNE      TASAAS
096000010124     C     CEDLNP        ORNE      TASLNP
096100010124     C     CEDNRS        ORNE      TASNRS
096200010124     C     CEDNSP        ORNE      TASNSP
096300010124     C     CEDTBL        ORNE      TASTBL
096400950307     C                   MOVEL     '1'           FINCED
096500001019     C                   ENDIF
096600950307     C*
096700950307     C                   ENDSR
096800950307     C*------------------------------------------------------------------------*
096900990802     C*? CHKTAS - CONTROLLA VALIDITA' DEL RECORD DI TITAS
097000950307     C*------------------------------------------------------------------------*
097100950309     C     CHKTAS        BEGSR
097200950307     C*
097300950307     C                   MOVEL     'S'           WRECOK            1
097400000329     C*
097500001023     C* CONTROLLA LO STATO DELLA CONSEGNA IN RELAZIONE AL TIPO BOLLA
097600001023     C* ==> SE TRATTASI DI BOLLA DI RECUPERO (F3/A3/AP) STORICIZZO COMUNQUE
097700001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
097800001018     C     TASDCM        IFEQ      *ZEROS                                       *NON CONSEGNATA
097900001023     C     TASTBL        ANDNE     'F3'
098000001023     C     TASTBL        ANDNE     'A3'
098100001023     C     TASTBL        ANDNE     'AP'
098200010222     C     TASCBO        ANDNE     'E '
098300010222     C     TASCBO        ANDNE     'B '
098400001019     C                   EXCEPT    ERRDCM
098500000329     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
098600001018     C                   ENDIF
098700001018     C                   ENDIF
098800950309     C*
098900950309     C* CONTROLLA IL CONTRASSEGNO
099000001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
099100950309     C     TASFCA        IFEQ      'S'                                          --- 1 -->
099200950309     C                   Z-ADD     TASAAS        CSBAAS
099300950309     C                   Z-ADD     TASLNP        CSBLNP
099400950309     C                   Z-ADD     TASNRS        CSBNRS
099500950309     C                   Z-ADD     TASNSP        CSBNSP
099600950309     C     KEYCSB        CHAIN     TNCSB03L                           99
099700950309     C     *IN99         IFEQ      '0'                                          --- 2 -->
099800950309     C     CSBSTA        IFNE      9                                            --- 3 -->
099900950309     C     CSBBNA        IFEQ      *ZEROS                                       *NON ANCORA PAGATO
100000950309     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
100100021021     C***                EXCEPT    NOCCSB                                       *STAMPO RECORD
100200021021     C                   ADD       1             CNTCSB                         *CONTEGGIO RECORD
100300950309     C                   ENDIF
100400950309     C                   ENDIF                                                  <-- 3 ---
100500950309     C                   ENDIF                                                  <-- 2 ---
100600950309     C                   ENDIF                                                  <-- 1 ---
100700001018     C                   ENDIF                                                  <-- 1 ---
100800000327     C*
100900000327     C* CONTROLLA I DANNI
101000001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
101100001018     C     TASFDA        IFEQ      'S'
101200040715      **
101300040715      **  Sostituito con il pgm FIDN12R che restituisce l'informazione
101400040715      **   sui danni fra bolla madre e figlia
101500040715      **
101600040715     C**                 Z-ADD     TASAAS        KCTAAS
101700040715     C**                 Z-ADD     TASLNP        KCTLNP
101800040715     C**                 Z-ADD     TASNRS        KCTNRS
101900040715     C**                 Z-ADD     TASNSP        KCTNSP
102000040715     C**   KEYDCT        CHAIN     FNDCT02L                           99
102100040715      *
102200040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
102300040715      * SIA COME MAMMA CHE COME FIGLIA
102400040715     c                   clear                   fidn12ds
102500040715     c                   eval      i12aas = TASAAS
102600040715     c                   eval      i12lnp = TASLNP
102700040715     c                   eval      i12nrs = TASNRS
102800040715     c                   eval      i12nsp = TASNSP
102900040715     c                   eval      i12fel = 'E'
103000040715     c                   call      'FIDN12R'
103100040715     c                   parm                    fidn12ds
103200040715      **
103300040715      * se c'è almeno una C.A. aperta
103400040715      **
103500040715     c     o12nca        ifgt      0
103600040715     C**   *IN99         IFEQ      *OFF                                         *TROVATO DANNO
103700040715      **
103800000327     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
103900021021     C***                EXCEPT    NOCDCT                                       *STAMPO RECORD
104000021021     C                   ADD       1             CNTDCT                         *CONTEGGIO RECORD
104100001018     C                   ENDIF
104200001018     C                   ENDIF
104300001018     C                   ENDIF
104400000404     C*
104500000404     C* CONTROLLA LE GIACENZE
104600001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
104700001018     C     TASFGC        IFEQ      'S'
104800000404     C                   Z-ADD     TASAAS        KCPAAS
104900000404     C                   Z-ADD     TASLNP        KCPLNP
105000000404     C                   Z-ADD     TASNRS        KCPNRS
105100000404     C                   Z-ADD     TASNSP        KCPNSP
105200000404     C                   Z-ADD     0             KCPFRG                         *PRIMO PRG
105300050301     C     KEYGCP        CHAIN(N)  TIGCP51L                           99
105400001018     C     *IN99         IFEQ      *OFF
105500001018     C     GCPFAS        IFLT      50                                           *FASI APERTE
105600000404     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
105700021021     C***                EXCEPT    NOCGCP                                       *STAMPO RECORD
105800021021     C                   ADD       1             CNTGCP                         *CONTEGGIO RECORD
105900001018     C                   ENDIF
106000001018     C                   ENDIF
106100001018     C                   ENDIF
106200001018     C                   ENDIF
106300011227     C*
106400020220     C* SE BOLLA POSTA CANCELLARE SOLO SE + VECCHIA D 2 ANNI
106500011227     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
106600011227     C     TASTSP        IFEQ      'P'                                          *BOLLA POSTA
106700020220     C     TASDSP        ANDGT     DSDATFP
106800011227     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
106900011227     C                   ENDIF
107000011227     C                   ENDIF
107100001019     C*
107200001019     C* CONTEGGIO BOLLE SCARTATE
107300001019     C     WRECOK        IFEQ      'N'
107400001019     C                   ADD       1             NUMNON
107500001019     C                   ENDIF
107600950307     C*
107700950307     C                   ENDSR
107800950309     C*------------------------------------------------------------------------*
107900950309     C*?AGGTAB - AGGIORNA LA TABELLA '$3' CON ULTIMA DATA TRASF. TAS00 > TAS10
108000950309     C*------------------------------------------------------------------------*
108100950309     C     AGGTAB        BEGSR
108200950309     C*
108300950309     C                   OPEN      TABEL00F
108400950309     C*
108500950309     C                   Z-ADD     1             KTBUTE
108600950309     C                   MOVEL     '$3'          KTBCOD
108700950309     C                   MOVEL     '1'           KTBKEY
108800950309     C     KEYTAB        CHAIN     TABEL00F                           99
108900950309     C     *IN99         IFEQ      *ON                                          *NON TROVATA
109000950309     C                   ELSE                                                   *TROVATA
109100950309     C                   MOVEL     TBLUNI        DS$3
109200950309     C                   Z-ADD     PARDTF        §$3DP0                         *ULT.DATA TRASF P>SA
109300020220     C                   Z-ADD     PARDFP        §$3DPP                         *ULT.DATA TRASF P>SA
109400950309     C                   MOVEL     DS$3          TBLUNI
109500950309     C                   UPDATE    TABEL
109600950309     C                   ENDIF
109700950309     C*
109800950309     C                   CLOSE     TABEL00F
109900950309     C*
110000950309     C                   ENDSR
110100950717     C*------------------------------------------------------------------------*
110200950717     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
110300950717     C*------------------------------------------------------------------------*
110400950717     C     SUBDAT        BEGSR
110500950717     C                   TIME                    DTTIME
110600950717     C                   Z-ADD     DTANN         DSANN
110700950717     C                   Z-ADD     DTMES         DSMES
110800950717     C                   Z-ADD     DTGIO         DSGIO
110900950717     C                   Z-ADD     DTSEC         DSSEC
111000950717     C                   ENDSR
111100940912     C*------------------------------------------------------------------------*
111200050705     C*? funzione per scivere file x Laguna
111300940912     C*------------------------------------------------------------------------*
111400050705     C     srlaguna      BEGSR
111500050705     c                   clear                   tnsb34ds
111600050929     c* ricavo il progressivo da impostare nella directory ogni 450000 bolle
111700050720     c* cancellate cambio directory
111800120709      * 09/07/2012 con Bocchi verificato che il progressivo non serve più
111900120709     C**!!!              ADD       1             NUMlag                         *REC TRASF TITAS
112000120709     c**!!!numlag        div       450000        sb34prg
112100120709     c**!!!              add       1             sb34prg
112200050705     c                   eval      sb34tla = 'R'
112300050705     c                   eval      sb34aas = tasaas
112400050705     c                   eval      sb34lnp = taslnp
112500050705     c                   eval      sb34nrs = tasnrs
112600050705     c                   eval      sb34nsp = tasnsp
112700050706     c                   eval      sb34dtp = pardtf
112800050706     c                   eval      sb34cop = parcop
112900050705     c                   call      'TNSB34R'
113000050705     c                   parm                    tnsb34ds
113100050705     c                   if        sb34err <> ' '
113200050705     c                   end
113300050705     C                   ENDSR
113400120709     C*------------------------------------------------------------------------*
113500120709     C*? funzione per scivere file x JDOC
113600120709     C*------------------------------------------------------------------------*
113700120709     C     srJdoc        BEGSR
113800120709     c                   clear                   tnsb34ds
113900120709     c                   eval      sb34tla = 'R'
114000120709     c                   eval      sb34aas = tasaas
114100120709     c                   eval      sb34lnp = taslnp
114200120709     c                   eval      sb34nrs = tasnrs
114300120709     c                   eval      sb34nsp = tasnsp
114400120709     c                   eval      sb34dtp = pardtf
114500120709     c                   eval      sb34cop = parcop
114600120709     c                   call      'TNSB34R1'
114700120709     c                   parm                    tnsb34ds
114800120709     c                   if        sb34err <> ' '
114900120709     c                   end
115000120709     C                   ENDSR
115100061122     C*------------------------------------------------------------------------*
115200061122     C*? WRTTASSTO - SCRITTURA FILE STORICO TITASS
115300061122     C*------------------------------------------------------------------------*
115400061122     C     WRTTASSTO     BEGSR
115500080312     c* memorizzo directory laguna per sapere dove è stata archiviata la spe
115600080312     c                   clear                   tasdtfp
115700080312     c                   if        §floiml <> ' '
115800080312     c                   move      tasaas        ladaas
115900080312     c                   move      taslnp        ladlnp
116000080312     c                   move      tasnrs        ladnrs
116100080312     c                   move      tasnsp        ladnsp
116200080312     c     klad          chain     tilad01l
116300080312     c                   If        %found(tilad01l)
116400080312     c                   movel     laddir        tasdtfp
116500080312     c                   end
116600080312     c                   end
116700061122      * Decodifica padroncino
116800061122     c                   If        taspdc <> *zeros
116900061122     c     Keyapd        chain     fiapd01l
117000061122     c                   If        %found(fiapd01l)
117100061122     c                   eval      taspdd = apdrsc
117200061122     c                   endif
117300061122     c                   endif
117400061122
117500061122      * recupero c/assegno
117600061122     c                   if        tasfca = 'S'
117700061122     c                   z-add     csbcas        Tascas
117800061122     c                   movel     csbvca        tasvca
117900061122     c                   endif
118000061122
118100061122     c                   move      tasksc        com04
118200061122      * PARTITA IVA / CODICE FISCALE
118300061122     c                   If        %subst(tastbl:1:1) = 'A'  and
118400061122     c                             tascpd <> *blanks
118500061122     c                   eval      tasiva  = tascpd
118600061122     c                   endif
118700061122      * se partita iva non valorizzata
118800061122     c                   If        tasiva = *blanks  or tascdf = *blanks
118900061122      * codificato franco o assegnato
119000061122     c                   if        com04  <>  9999  and com04  <>  8888
119100061122     c                   exsr      reciva
119200061122     c                   if        o69err <> *on
119300061122     c                   if        tascdf = *blanks
119400061122     c                   eval      tasiva = indiva
119500061122     c                   endif
119600061122      * se devo cercare anche il codife fiscale
119700061122     c                   if        tascdf = *blanks
119800061122     c                   eval      tascdf = indcdf
119900061122     c                   endif
120000061122
120100061122     c                   endif
120200061122     c                   endif
120300061122     c                   endif
120400061124      * valorizzo il flag che mi identifica le spedizioni senza segnacollo
120500091229     c                   If        Tasfns <> 'S' and tasncd = *zeros
120600061124     c                   eval      tassss = 'S'
120700061124     c                   endif
120800061122
120900061122     c                   write     titassto                             99
121000061122     C   99              EXCEPT    ERRsto
121100061122     C   99              MOVEL     '1'           FINL00                         *FINE PGM
121200061122     C  N99              ADD       1             NUMsto                         *REC TRASF FNEVB
121300061122
121400061122     C                   ENDSR
121500061122     C*
121600050705     C*------------------------------------------------------------------------*
121700050705     C*? RUTINZ - ROUTINE INIZIALE
121800050705     C*------------------------------------------------------------------------*
121900050705     C     RUTINZ        BEGSR
122000931115     C*
122100931115     C* PARAMETRI IN INPUT AL PGM
122200931115     C     *ENTRY        PLIST
122300931115     C                   PARM                    KPJBA
122400931115     C                   MOVEL     KPJBU         PARAM
122500950309     C*
122600950309     C* DEFINISCO CAMPI
122700950309     C     *LIKE         DEFINE    TBLKUT        KTBUTE
122800950309     C     *LIKE         DEFINE    TBLCOD        KTBCOD
122900950309     C     *LIKE         DEFINE    TBLKEY        KTBKEY
123000970903     C     *LIKE         DEFINE    CETNBC        MINNBC
123100040715     C***  *LIKE         DEFINE    DCTAAS        KCTAAS                         *FNDCT00F
123200040715     C***  *LIKE         DEFINE    DCTLNP        KCTLNP
123300040715     C***  *LIKE         DEFINE    DCTNRS        KCTNRS
123400040715     C***  *LIKE         DEFINE    DCTNSP        KCTNSP
123500050301     C     *LIKE         DEFINE    GCPAAS        KCPAAS                         *TIGCP00F
123600000404     C     *LIKE         DEFINE    GCPLNP        KCPLNP
123700000404     C     *LIKE         DEFINE    GCPNRS        KCPNRS
123800000404     C     *LIKE         DEFINE    GCPNSP        KCPNSP
123900000404     C     *LIKE         DEFINE    GCPFRG        KCPFRG
124000050301     C     *LIKE         DEFINE    GAPAAS        KAPAAS                         *TIGAP00F
124100000404     C     *LIKE         DEFINE    GAPLNP        KAPLNP
124200000404     C     *LIKE         DEFINE    GAPNRS        KAPNRS
124300000404     C     *LIKE         DEFINE    GAPNSP        KAPNSP
124400050301     C     *LIKE         DEFINE    GNPAGC        KNPAGC                         *TIGNP00F
124500000404     C     *LIKE         DEFINE    GNPFGC        KNPFGC
124600000404     C     *LIKE         DEFINE    GNPNGC        KNPNGC
124700950309     C*
124800950309     C* CHIAVE LETTURA TABEL00F - COMPLETA
124900950309     C     KEYTAB        KLIST
125000950309     C                   KFLD                    KTBUTE
125100950309     C                   KFLD                    KTBCOD
125200950309     C                   KFLD                    KTBKEY
125300080925     C     KEYTAB1       KLIST
125400080925     C                   KFLD                    KTBUTE
125500080925     C                   KFLD                    KTBCOD
125600080925     C                   Z-ADD     1             KTBUTE
125700080925     C                   MOVEL     'TB'          KTBCOD
125800950309     C*
125900950309     C* CHIAVE LETTURA TNCSB01L - COMPLETA
126000950309     C     KEYCSB        KLIST
126100950309     C                   KFLD                    CSBAAS
126200950309     C                   KFLD                    CSBLNP
126300950309     C                   KFLD                    CSBNRS
126400950309     C                   KFLD                    CSBNSP
126500000327     C*
126600000327     C* CHIAVE LETTURA FNDCT02L - COMPLETA
126700040715     C***  KEYDCT        KLIST
126800040715     C***                KFLD                    KCTAAS
126900040715     C***                KFLD                    KCTLNP
127000040715     C***                KFLD                    KCTNRS
127100040715     C***                KFLD                    KCTNSP
127200930208     C*
127300990802     C* CHIAVE LETTURA TITAS38C - PARZIALE
127400930621     C     KEYTAS        KLIST
127500941123     C                   KFLD                    TASAAS
127600941123     C                   KFLD                    TASMGS
127700950307     C*
127800990802     C* CHIAVE LETTURA TITA730C - COMPLETA
127900950307     C     KEYTA7        KLIST
128000950307     C                   KFLD                    TA7AAS
128100950307     C                   KFLD                    TA7LNP
128200950307     C                   KFLD                    TA7NRS
128300950307     C                   KFLD                    TA7NSP
128400950307     C                   KFLD                    TA7TBL
128500990802     C*
128600990802     C* CHIAVE LETTURA TITAA30C - COMPLETA
128700990802     C     KEYTAA        KLIST
128800990802     C                   KFLD                    TAAAAS
128900990802     C                   KFLD                    TAALNP
129000990802     C                   KFLD                    TAANRS
129100990802     C                   KFLD                    TAANSP
129200990802     C*
129300990802     C* CHIAVE LETTURA TITAT30C - COMPLETA
129400990802     C     KEYTAT        KLIST
129500990802     C                   KFLD                    TATAAS
129600990802     C                   KFLD                    TATLNP
129700990802     C                   KFLD                    TATNRS
129800990802     C                   KFLD                    TATNSP
129900990802     C*
130000990802     C* CHIAVE LETTURA TITA430C - COMPLETA
130100990802     C     KEYTA4        KLIST
130200990802     C                   KFLD                    TA4AAS
130300990802     C                   KFLD                    TA4LNP
130400990802     C                   KFLD                    TA4NRS
130500990802     C                   KFLD                    TA4NSP
130600010903     C*
130700010903     C* CHIAVE LETTURA TITAH30C - COMPLETA
130800010903     C     KEYTAH        KLIST
130900010903     C                   KFLD                    TAHAAS
131000010903     C                   KFLD                    TAHLNP
131100010903     C                   KFLD                    TAHNRS
131200010903     C                   KFLD                    TAHNSP
131300950209     C*
131400950307     C* CHIAVE LETTURA ECCET30C - COMPLETA
131500950220     C     KEYCET        KLIST
131600950220     C                   KFLD                    CETAAS                         ANNO SPEDIZIONE
131700950220     C                   KFLD                    CETLNP                         LINEA DI PARTENZA
131800950220     C                   KFLD                    CETNRS                         NUMERO SERIE
131900950220     C                   KFLD                    CETNSP                         NUMERO SPEDIZIONE
132000950220     C                   KFLD                    CETTBL                         TIPO BOLLA
132100950307     C*
132200950307     C* CHIAVE LETTURA ECCED30C - COMPLETA
132300950307     C     KEYCED        KLIST
132400950307     C                   KFLD                    CEDAAS                         ANNO SPEDIZIONE
132500950307     C                   KFLD                    CEDLNP                         LINEA DI PARTENZA
132600950307     C                   KFLD                    CEDNRS                         NUMERO SERIE
132700950307     C                   KFLD                    CEDNSP                         NUMERO SPEDIZIONE
132800950307     C                   KFLD                    CEDTBL                         TIPO BOLLA
132900980325     C*
133000980325     C* CHIAVE LETTURA ECFIM30C - COMPLETA
133100980325     C     KEYFIM        KLIST
133200980325     C                   KFLD                    FIMAAS                         ANNO SPEDIZIONE
133300980325     C                   KFLD                    FIMLNP                         LINEA DI PARTENZA
133400980325     C                   KFLD                    FIMNRS                         NUMERO SERIE
133500980325     C                   KFLD                    FIMNSP                         NUMERO SPEDIZIONE
133600980325     C                   KFLD                    FIMTBL                         TIPO BOLLA
133700000327     C*
133800000327     C* CHIAVE LETTURA FNEVB01L - COMPLETA
133900000327     C     KEYEVB        KLIST
134000000327     C                   KFLD                    EVBAAS                         ANNO SPEDIZIONE
134100000327     C                   KFLD                    EVBLNP                         LINEA DI PARTENZA
134200000327     C                   KFLD                    EVBNRS                         NUMERO SERIE
134300000327     C                   KFLD                    EVBNSP                         NUMERO SPEDIZIONE
134400000404     C*
134500050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
134600000404     C     KEYGCP        KLIST
134700000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
134800000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
134900000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
135000000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
135100000404     C                   KFLD                    KCPFRG                         PRG RIAPERTURE
135200000404     C*
135300050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
135400000404     C     KE2GCP        KLIST
135500000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
135600000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
135700000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
135800000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
135900000404     C*
136000050301     C* CHIAVE LETTURA TIGAP01L - PARZIALE
136100000404     C     KEYGAP        KLIST
136200000404     C                   KFLD                    KAPAAS                         ANNO SPEDIZIONE
136300000404     C                   KFLD                    KAPLNP                         LINEA DI PARTENZA
136400000404     C                   KFLD                    KAPNRS                         NUMERO SERIE
136500000404     C                   KFLD                    KAPNSP                         NUMERO SPEDIZIONE
136600000404     C*
136700050301     C* CHIAVE LETTURA TIGNP01L - PARZIALE
136800000404     C     KEYGNP        KLIST
136900000404     C                   KFLD                    KNPAGC                         ANNO APERTURA
137000000404     C                   KFLD                    KNPFGC                         FILILAE APERTURA
137100000404     C                   KFLD                    KNPNGC                         NUMERO GIACENZA
137200040303     C*
137300040531     C* CHIAVE LETTURA FIAR531C
137400040303     C     KEYAR5        KLIST
137500040303     C                   KFLD                    AR5AAS
137600040303     C                   KFLD                    AR5LNP
137700040303     C                   KFLD                    AR5NRS
137800040303     C                   KFLD                    AR5NSP
137900120709
138000120709     C     KEYAR51       KLIST
138100120709     C                   KFLD                    AR5AAS
138200120709     C                   KFLD                    AR5LNP
138300120709     C                   KFLD                    AR5NRS
138400120709     C                   KFLD                    AR5NSP
138500120709     C                   KFLD                    AR5TRD
138600060919     C*
138700060919     C* CHIAVE LETTURA TITAI01L
138800060919     C     KEYtai        KLIST
138900060919     C                   KFLD                    taiAAS
139000060919     C                   KFLD                    taiLNP
139100060919     C                   KFLD                    taiNRS
139200060919     C                   KFLD                    taiNSP
139300060919     C                   KFLD                    taiTBL
139400081103
139500081103     C* CHIAVE LETTURA TITAI01L
139600081103     C     KEYrab        KLIST
139700081103     C                   KFLD                    rabAAS
139800081103     C                   KFLD                    rabLNP
139900081103     C                   KFLD                    rabNRS
140000081103     C                   KFLD                    rabNSP
140100061122
140200061122     C* CHIAVE LETTURA Fiapd01l
140300061122     C     KEYapd        KLIST
140400061122     C                   KFLD                    trcapd
140500061122     C                   KFLD                    taspdc
140600040823      *
140700040823     C     kbol          klist
140800040823     C                   kfld                    tasaas
140900040823     C                   kfld                    taslnp
141000040823     C                   kfld                    tasnrs
141100040823     C                   kfld                    tasnsp
141200080312      *
141300080312     C     klad          klist
141400080312     C                   kfld                    LADAAS
141500080312     C                   kfld                    LADLNP
141600080312     C                   kfld                    LADNRS
141700080312     C                   kfld                    LADNSP
141800000327     C*
141801131029     C     Kevb2f        KLIST
141802131029     C                   KFLD                    EVBAAS
141803131029     C                   KFLD                    EVBLNP
141804131029     C                   KFLD                    EVBNRS
141805131029     C                   KFLD                    EVBNSP
141806131029     C                   KFLD                    EVBDTV
141807131029     C                   KFLD                    EVBORV
141808131029     C                   KFLD                    EVBCEV
141900000327     C* CALCOLA LA DATA CORRENTE
142000000327     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
142100000327     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
142200000327     C                   Z-ADD     N8            G08DAT
142300000327     C                   Z-ADD     *ZEROS        G08INV
142400000327     C                   MOVEL     '0'           G08ERR
142500000327     C                   CALL      'XSRDA8'
142600000327     C                   PARM                    WLBDA8
142700000327     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
142800930208     C*
142900930208     C* IMPOSTO A ZERO IL FINE PGM
143000930208     C                   MOVEL     '0'           FINL00            1
143100931105     C                   MOVEL     *BLANKS       RSUT             20
143200040922     C                   Z-ADD     *ZEROS        NUMblp            9 0
143300040922     C                   Z-ADD     *ZEROS        NUMarb            9 0
143400950308     C                   Z-ADD     *ZEROS        NUMTAS            9 0
143500120709     C**!!!              Z-ADD     *ZEROS        NUMlag            9 0
143600950308     C                   Z-ADD     *ZEROS        NUMTA7            9 0
143700990802     C                   Z-ADD     *ZEROS        NUMTAA            9 0
143800990802     C                   Z-ADD     *ZEROS        NUMTAT            9 0
143900990802     C                   Z-ADD     *ZEROS        NUMTA4            9 0
144000010903     C                   Z-ADD     *ZEROS        NUMTAH            9 0
144100950307     C                   Z-ADD     *ZEROS        NUMCET            9 0
144200950308     C                   Z-ADD     *ZEROS        NUMCED            9 0
144300980325     C                   Z-ADD     *ZEROS        NUMFIM            9 0
144400970904     C                   Z-ADD     *ZEROS        NUMECT            9 0
144500970904     C                   Z-ADD     *ZEROS        NUMECD            9 0
144600000327     C                   Z-ADD     *ZEROS        NUMEVB            9 0
144700000404     C                   Z-ADD     *ZEROS        NUMGCP            9 0
144800000404     C                   Z-ADD     *ZEROS        NUMGAP            9 0
144900000404     C                   Z-ADD     *ZEROS        NUMGNP            9 0
145000001019     C                   Z-ADD     *ZEROS        NUMNON            9 0
145100040303     C                   Z-ADD     *ZEROS        NUMAR5            9 0
145200060919     C                   Z-ADD     *ZEROS        NUMtai            9 0
145300081103     C                   Z-ADD     *ZEROS        NUMrab            9 0
145400061122     C                   Z-ADD     *ZEROS        NUMsto            9 0
145500021021     C                   Z-ADD     *ZEROS        CNTGCP            9 0
145600021021     C                   Z-ADD     *ZEROS        CNTDCT            9 0
145700021021     C                   Z-ADD     *ZEROS        CNTCSB            9 0
145800040823      * Apro il Fnblp di filiale
145900040823     c                   Clear                   comman
146000040823     c                   Movel(p)  cmd(1)        comman
146100040823     c                   Eval      %Subst(comman:13:8) = 'FNBLP01L'
146200040910      *test sistema informativo di prova
146300040910     c                   if        %subst(knsif:7:1) = 'P'
146400040910     c                   Eval      %Subst(comman:30:10) = wlibp
146500040910     c                   else
146600040910     c                   Eval      %Subst(comman:30:10) = wlib
146700040910     c                   endif
146800040910
146900040823     c                   Eval      comman =
147000040823     c                             %trim(comman) + '/FNBLP01L)'
147100040823     c                   Call      'QCMDEXC'                            99
147200040823     c                   Parm                    comman
147300040823     c                   Parm                    lenght
147400040823     c                   If        *In99
147500040823     c                   seton                                        lr
147600040823     c                   Else
147700040823     c                   Open      Fnblp01l
147800040823     c                   EndIf
147900040823      *
148000040823      * Apro  Fnarb di filiale
148100040823     c                   Clear                   comman
148200040823     c                   Movel(p)  cmd(1)        comman
148300040823     c                   Eval      %Subst(comman:13:8) = 'FNARB01L'
148400040910      *test sistema informativo di prova
148500040910     c                   if        %subst(knsif:7:1) = 'P'
148600040910     c                   Eval      %Subst(comman:30:10) = wlibp
148700040910     c                   else
148800040910     c                   Eval      %Subst(comman:30:10) = wlib
148900040910     c                   endif
149000040910
149100040823     c                   Eval      comman =
149200040823     c                             %trim(comman) + '/FNARB01L)'
149300040823     c                   Call      'QCMDEXC'                            99
149400040823     c                   Parm                    comman
149500040823     c                   Parm                    lenght
149600040823     c                   If        *In99
149700040823     c                   seton                                        lr
149800040823     c                   Else
149900040823     c                   Open      Fnarb01l
150000040823     c                   EndIf
150100040823      *
150200931115     C*
150300950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
150400080925     c* carico i tipi bolla RECUPERO per evitare di salvarli nel TITASS0F
150500081103     c                   clear                   conta
150600080926     c                   open      tabel00f
150700080925     c     keytab1       setll     tabel00f
150800080925     c                   do        *hival
150900080925     c     keytab1       reade(n)  tabel00f
151000080925     c                   if        %eof(tabel00f)
151100080925     c                   leave
151200080925     c                   end
151300080925     c                   movel(p)  tbluni        dstb
151400080925     c                   if        §TBRBL ='R'
151500080925     c                   add       1             conta             2 0
151600080925     c                   movel     tblkey        tbl(conta)
151700080925     c                   end
151800080925     c                   enddo
151900080926     c                   close     tabel00f
152000000000     C*
152100000000     C                   ENDSR
152200940912     O*------------------------------------------------------------------------*
152300940905     OPRTF198   E            TESTA            02
152400950309     O                                            8 'TNSB38R '
152500931105     O                       RSUT                45
152600990802     O                                           82 'TRASF. TITASP -> SALVAT.'
152700931115     O                                           90 'DAL:'
152800950309     O                       PARDTI              99
152900931115     O                                          110 'AL:'
153000950309     O                       PARDTF             119
153100020220     O                                          128 '/PT'
153200020220     O                       PARDFP             132
153300931105     O                                          150 'Data:'
153400950717     O                       DSDAT6             159 '  /  /  '
153500931105     O                                          167 'Ora:'
153600950717     O                       DTORA6             178 '  .  .  '
153700931105     O                                          190 'Pagina:'
153800931105     O                       PAGE          Z    198
153900931105     O          E            TESTA       1  1
154000931105     O                       TES(2)              66
154100931105     O                       TES(3)             132
154200931105     O                       TES(4)             198
154300931105     O          E            TESTA          1
154400931105     O                       TES(1)              66
154500931105     O                       TES(1)             132
154600931105     O                       TES(1)             198
154700001019     O*------------------------------------
154800001019     O          E            ERRDCM         1
154900001019     O                       TASAAS            +  5
155000001019     O                       TASLNP            +  1
155100001019     O                       TASNRS            +  1
155200001019     O                       TASNSP            +  1
155300001019     O                       TASTBL            +  1
155400001019     O                                         +  1 'BOLLA NON CONS.    '
155500931105     O*------------------------------------
155600950307     O          E            ERRTAS         1
155700950307     O                       TASAAS            +  5
155800950307     O                       TASLNP            +  1
155900950307     O                       TASNRS            +  1
156000950307     O                       TASNSP            +  1
156100950307     O                       TASTBL            +  1
156200990802     O                                         +  1 'ERR. WRITE TITAS   '
156300950307     O*------------------------------------
156400950307     O          E            ERRTA7         1
156500950307     O                       TA7AAS            +  5
156600950307     O                       TA7LNP            +  1
156700950307     O                       TA7NRS            +  1
156800950307     O                       TA7NSP            +  1
156900950307     O                       TA7TBL            +  1
157000950307     O                       TA7SVN            +  1
157100990802     O                                         +  1 'ERR. WRITE TITA7   '
157200990802     O*------------------------------------
157300990802     O          E            ERRTAA         1
157400990802     O                       TAAAAS            +  5
157500990802     O                       TAALNP            +  1
157600990802     O                       TAANRS            +  1
157700990802     O                       TAANSP            +  1
157800990802     O                       TAATRC            +  1
157900990802     O                                         +  1 'ERR. WRITE TITAA   '
158000990802     O*------------------------------------
158100990802     O          E            ERRTAT         1
158200990802     O                       TATAAS            +  5
158300990802     O                       TATLNP            +  1
158400990802     O                       TATNRS            +  1
158500990802     O                       TATNSP            +  1
158600990802     O                       TATNSC            +  1
158700990802     O                                         +  1 'ERR. WRITE TITAT   '
158800990802     O*------------------------------------
158900990802     O          E            ERRTA4         1
159000990802     O                       TA4AAS            +  5
159100990802     O                       TA4LNP            +  1
159200990802     O                       TA4NRS            +  1
159300990802     O                       TA4NSP            +  1
159400990802     O                       TA4TRC            +  1
159500990802     O                                         +  1 'ERR. WRITE TITA4   '
159600010903     O*------------------------------------
159700010903     O          E            ERRTAH         1
159800010903     O                       TAHAAS            +  5
159900010903     O                       TAHLNP            +  1
160000010903     O                       TAHNRS            +  1
160100010903     O                       TAHNSP            +  1
160200010903     O                       TAHTRC            +  1
160300010903     O                                         +  1 'ERR. WRITE TITAH   '
160400950307     O*------------------------------------
160500950307     O          E            ERRCET         1
160600950307     O                       CETAAS            +  5
160700950307     O                       CETLNP            +  1
160800950307     O                       CETNRS            +  1
160900950307     O                       CETNSP            +  1
161000950307     O                       CETTBL            +  1
161100950309     O                                         +  1 'ERR. WRITE ECCET   '
161200950307     O*------------------------------------
161300950307     O          E            ERRCED         1
161400950307     O                       CEDAAS            +  5
161500950307     O                       CEDLNP            +  1
161600950307     O                       CEDNRS            +  1
161700950307     O                       CEDNSP            +  1
161800950307     O                       CEDTBL            +  1
161900950307     O                       CEDCMP            +  1
162000950309     O                                         +  1 'ERR. WRITE ECCED   '
162100980325     O*------------------------------------
162200980325     O          E            ERRFIM         1
162300980325     O                       FIMAAS            +  5
162400980325     O                       FIMLNP            +  1
162500980325     O                       FIMNRS            +  1
162600980325     O                       FIMNSP            +  1
162700980325     O                       FIMTBL            +  1
162800980325     O                                         +  1 'ERR. WRITE ECFIM   '
162900970903     O*------------------------------------
163000970903     O          E            ERRECT         1
163100970903     O                       ECTNBC        Z   +  5
163200970903     O                       ECTCMP        Z   +  1
163300970903     O                       ECTDDC            +  1 '    /  /  '
163400970903     O                                         +  1 'ERR. WRITE ECECT   '
163500970903     O*------------------------------------
163600970903     O          E            ERRECD         1
163700970903     O                       ECDNBC        Z   +  5
163800970903     O                       ECDCMP        Z   +  1
163900970903     O                       ECDFRA            +  1
164000970903     O                       ECDIOE            +  1
164100970903     O                       ECDKCC        Z   +  1
164200970903     O                       ECDKSC        Z   +  1
164300970903     O                       ECDRIC            +  1
164400970903     O                       ECDCCF        Z   +  1
164500970903     O                       ECDDDO            +  1 '    /  /  '
164600970903     O                                         +  1 'ERR. WRITE ECECD   '
164700000404     O*------------------------------------
164800000404     O          E            ERREVB         1
164900000404     O                       EVBAAS            +  5
165000000404     O                       EVBLNP            +  1
165100000404     O                       EVBNRS            +  1
165200000404     O                       EVBNSP            +  1
165300000404     O                                         +  1 'ERR. WRITE FNEVB   '
165400000404     O*------------------------------------
165500000404     O          E            ERRGCP         1
165600000404     O                       GCPAAS            +  5
165700000404     O                       GCPLNP            +  1
165800000404     O                       GCPNRS            +  1
165900000404     O                       GCPNSP            +  1
166000050301     O                                         +  1 'ERR. WRITE TIGCP   '
166100050303     O          E            ERRGCP1        1
166200050303     O                       GCPAAS            +  5
166300050303     O                       GCPLNP            +  1
166400050303     O                       GCPNRS            +  1
166500050303     O                       GCPNSP            +  1
166600050303     O                                         +  1 'GIACENZA ANCORA IN FILIALE'
166700000404     O*------------------------------------
166800000404     O          E            ERRGAP         1
166900000404     O                       GAPAAS            +  5
167000000404     O                       GAPLNP            +  1
167100000404     O                       GAPNRS            +  1
167200000404     O                       GAPNSP            +  1
167300050301     O                                         +  1 'ERR. WRITE TIGAP   '
167400000404     O*------------------------------------
167500000404     O          E            ERRGNP         1
167600000404     O                       GNPAGC            +  5
167700000404     O                       GNPFGC            +  4
167800000404     O                       GNPNGC            +  1
167900050301     O                                         +  1 'ERR. WRITE TIGNP   '
168000040303     O*------------------------------------
168100040303     O          E            ERRAR5         1
168200040303     O                       AR5AAS            +  5
168300040303     O                       AR5LNP            +  1
168400040303     O                       AR5NRS            +  1
168500040303     O                       AR5NSP            +  1
168600040303     O                                         +  1 'ERR. WRITE FIAR5   '
168700060919     O          E            ERRtai         1
168800060919     O                       taiAAS            +  5
168900060919     O                       taiLNP            +  1
169000060919     O                       taiNRS            +  1
169100060919     O                       taiNSP            +  1
169200060919     O                       taitbl            +  1
169300060919     O                                         +  1 'ERR. WRITE TITAI   '
169400081103     O          E            ERRrab         1
169500081103     O                       rabAAS            +  5
169600081103     O                       rabLNP            +  1
169700081103     O                       rabNRS            +  1
169800081103     O                       rabNSP            +  1
169900081103     O                                         +  1 'ERR. WRITE FIRAB   '
170000950309     O*------------------------------------
170100000404     O          E            NOCCSB         1
170200950309     O                       CSBAAS            +  5
170300950309     O                       CSBLNP            +  1
170400950309     O                       CSBNRS            +  1
170500950309     O                       CSBNSP            +  1
170600000404     O                                         +  1 'BOLLA NON CANC:TNCSB'
170700000404     O*------------------------------------
170800040715     O*****     E            NOCDCT         1
170900040715     O*****                  DCTAAS            +  5
171000040715     O*****                  DCTLNP            +  1
171100040715     O*****                  DCTNRS            +  1
171200040715     O*****                  DCTNSP            +  1
171300040715     O*****                                    +  1 'BOLLA NON CANC:FNDCT'
171400040715     O          E            NOCDCT         1
171500040715     O                       i12AAS            +  5
171600040715     O                       i12LNP            +  1
171700040715     O                       i12NRS            +  1
171800040715     O                       i12NSP            +  1
171900040715     O                                         +  1 'BOLLA NON CANC:FNDCT'
172000000404     O*------------------------------------
172100000404     O          E            NOCGCP         1
172200000404     O                       GCPAAS            +  5
172300000404     O                       GCPLNP            +  1
172400000404     O                       GCPNRS            +  1
172500000404     O                       GCPNSP            +  1
172600050301     O                                         +  1 'BOLLA NON CANC:TIGCP'
172700040922     O*------------------------------------
172800040922     O          E            ERRblp         1
172900040922     O                       blpAAS            +  5
173000040922     O                       blpLNP            +  1
173100040922     O                       blpNRS            +  1
173200040922     O                       blpNSP            +  1
173300040922     O                                         +  1 'ERR. Trovato reK su FNBLP'
173400040922     O*------------------------------------
173500040922     O          E            ERRarb         1
173600040922     O                       arbAAS            +  5
173700040922     O                       arbLNP            +  1
173800040922     O                       arbNRS            +  1
173900040922     O                       arbNSP            +  1
174000040922     O                                         +  1 'ERR. Trovato reK su FNARB'
174100061122     O          E            ERRSTO         1
174200061122     O                       TASAAS            +  5
174300061122     O                       TASLNP            +  1
174400061122     O                       TASNRS            +  1
174500061122     O                       TASNSP            +  1
174600061122     O                       TASTBL            +  1
174700061122     O                                         +  1 'ERR. WRITE STORICO '
174800040922     O*------------------------------------
174900950308     O          E            STAFIN         1
175000990802     O                                           30 'N.REC.TRASF. TITAS'
175100950308     O                       NUMTAS        2     44
175200950308     O          E            STAFIN         1
175300990802     O                                           30 'N.REC.TRASF. TITA7'
175400950308     O                       NUMTA7        2     44
175500990802     O          E            STAFIN         1
175600990802     O                                           30 'N.REC.TRASF. TITAA'
175700990802     O                       NUMTAA        2     44
175800990802     O          E            STAFIN         1
175900990802     O                                           30 'N.REC.TRASF. TITAT'
176000990802     O                       NUMTAT        2     44
176100990802     O          E            STAFIN         1
176200990802     O                                           30 'N.REC.TRASF. TITA4'
176300990802     O                       NUMTA4        2     44
176400010903     O          E            STAFIN         1
176500010903     O                                           30 'N.REC.TRASF. TITAH'
176600010903     O                       NUMTAH        2     44
176700950308     O          E            STAFIN         1
176800950308     O                                           30 'N.REC.TRASF. ECCET'
176900950308     O                       NUMCET        2     44
177000950308     O          E            STAFIN         1
177100950308     O                                           30 'N.REC.TRASF. ECCED'
177200950308     O                       NUMCED        2     44
177300980325     O          E            STAFIN         1
177400980325     O                                           30 'N.REC.TRASF. ECFIM'
177500980325     O                       NUMFIM        2     44
177600970903     O          E            STAFIN         1
177700970903     O                                           30 'N.REC.TRASF. ECECT'
177800970903     O                       NUMECT        2     44
177900970903     O          E            STAFIN         1
178000970903     O                                           30 'N.REC.TRASF. ECECD'
178100970903     O                       NUMECD        2     44
178200000327     O          E            STAFIN         1
178300000327     O                                           30 'N.REC.TRASF. FNEVB'
178400000327     O                       NUMEVB        2     44
178500000404     O          E            STAFIN         1
178600050301     O                                           30 'N.REC.TRASF. TIGCP'
178700000404     O                       NUMGCP        2     44
178800000404     O          E            STAFIN         1
178900050301     O                                           30 'N.REC.TRASF. TIGAP'
179000000404     O                       NUMGAP        2     44
179100000404     O          E            STAFIN         1
179200050301     O                                           30 'N.REC.TRASF. TIGNP'
179300000404     O                       NUMGNP        2     44
179400040310     O          E            STAFIN         1
179500040310     O                                           30 'N.REC. BOLLE FIAR5'
179600040310     O                       NUMAR5        2     44
179700060919     O          E            STAFIN         1
179800060919     O                                           30 'N.REC. BOLLE TITAI'
179900060919     O                       NUMTAI        2     44
180000081103     O          E            STAFIN         1
180100081103     O                                           30 'N.REC. BOLLE FIRAB'
180200081103     O                       NUMRAB        2     44
180300061122     O          E            STAFIN         1
180400061122     O                                           30 'N.BOLLE IN STORICO'
180500061122     O                       NUMSTO        2     44
180600001019     O          E            STAFIN         1
180700001020     O                                           30 'N.REC. BOLLE SCART'
180800001019     O                       NUMNON        2     44
180900021021     O          E            STAFIN         1
181000021021     O                                           30 'DI CUI:'
181100021021     O          E            STAFIN         1
181200021021     O                                           30 'X GIACENZA APERTA '
181300021021     O                       CNTGCP        2     44
181400021021     O          E            STAFIN         1
181500021021     O                                           30 'X DANNO PENDENTE  '
181600021021     O                       CNTDCT        2     44
181700021021     O          E            STAFIN         1
181800021021     O                                           30 'X C/A ATTIVO      '
181900021021     O                       CNTCSB        2     44
182000040922     O          E            STAFIN         1
182100040922     O                                           30 'N.REK.Trovati FNBLP'
182200040922     O                       NUMblp        2     44
182300040922     O          E            STAFIN         1
182400040922     O                                           30 'N.REK.Trovati FNARB'
182500040922     O                       NUMarb        2     44
182600060626     O          E            STAFIN         1
182700060626     O                                           +0 'Chiusuara pgm: FINTAS'
182800060626     O                       fintas              +1
182900060626     O                       tasdsp              +1
183000060626     O                       dsdatf              +1
183100060626     O               98                          +1 'SHTDN'
183200060626     O                                           +1 'FINL00'
183300060626     O                       finl00              +1
183400950308     O          E            STAFIN           60
183500931115     O                                           76 '*** FINE STAMPA ***'
183600931115     O                                          150 'Data:'
183700950717     O                       DSDAT6             159 '  /  /  '
183800990802     O                                          167 'Ora:'
183900950717     O                       DTORA6             178 '  :  :  '
184000940912     O*------------------------------------------------------------------------*
184100970903     O* AGGIORNAMENTO FILES
184200970903     O*------------------------------------------------------------------------*
184300970903     OECECT     E            AGGECT
184400970903     O                       ECTATB
184500970903     O                       ECTFTR
184600970903     OECECD     E            AGGECD
184700970903     O                       ECDATB
184800970903     O                       ECDFTR
184900970903     O*------------------------------------------------------------------------*
185000040823** cmd
185100040823OVRDBF FILE(        ) TOFILE(
185200931105**  TES - RIGHE IN STAMPA
185300931105------------------------------------------------------------------  1
185400931105 DATA    ORA       AA LNP SE N.SPED. TB     ERR  DESCRIZIONE ERROR  2
185500950308E                                                                   3
185600931105                                                                    4
185700931105                                                                    5
185800931105                                                                    6
185900931105                                                                    7
186000931105                                                                    8
186100931105                                                                    9
186200931105                                                                   10
186300931105                                                                   11
186400931105                                                                   12
186500931105                                                                   13
186600931105                                                                   14
186700931105                                                                   15
