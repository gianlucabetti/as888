000100140107     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000200940912     H*------------------------------------------------------------------------*
000300971105     H* TRASFERIMENTO BOLLE NELLA LIBRERIA DI SALVATAGGIO
000400940912     H*------------------------------------------------------------------------*
000500930128     H DECEDIT('0,') DATEDIT(*DMY.)
000600990802     F*---
000700990802     F* BOLLE
000800990802     F*---
000900990802     FTITAS38C  UF   E           K DISK
001000990802     F                                     IGNORE(TITAS000)
001100990802     F                                     IGNORE(TITAS010)
001200990802     FTITASP0F  O  A E           K DISK
001300990802     F                                     RENAME(TITASP00:TITASS00)
001400061121     FTITASS0F  O  A E           K DISK
001500061121     F                                     RENAME(TITASS00:TITASSTO)
001600990802     F*---
001700990802     F* ESTENSIONE BOLLE
001800990802     F*---
001900990802     FTITA730C  UF   E           K DISK
002000990802     FTITA7P0F  O  A E           K DISK
002100990802     F                                     RENAME(TITA7P00:TITA7S00)
002200990802     F*---
002300990802     F* ANAGRAFICHE
002400990802     F*---
002500990802     FTITAA30C  UF   E           K DISK
002600990802     FTITAAP0F  O  A E           K DISK
002700990802     F                                     RENAME(TITAAP00:TITAAS00)
002800990802     F*---
002900990802     F* SEGNACOLLI
003000990802     F*---
003100990802     FTITAT30C  UF   E           K DISK
003200990802     FTITATP0F  O  A E           K DISK
003300990802     F                                     RENAME(TITATP00:TITATS00)
003400990802     F*---
003500990802     F* RIFERIMENTI
003600990802     F*---
003700990802     FTITA430C  UF   E           K DISK
003800990802     FTITA4P0F  O  A E           K DISK
003900990802     F                                     RENAME(TITA4P00:TITA4S00)
004000010903     F*---
004100010903     F* ESTENSIONE DETTAGLI SEGNACOLLI
004200010903     F*---
004300010903     FTITAH30C  UF   E           K DISK
004400010903     FTITAHP0F  O  A E           K DISK
004500010903     F                                     RENAME(TITAHP00:TITAHS00)
004600990802     F*---
004700990802     F* COMPETENZE TESTATA
004800990802     F*---
004900950307     FECCET30C  UF   E           K DISK
005000950307     FECCETP0F  O  A E           K DISK
005100010829     F                                     RENAME(ECCETP00:ECCETS)
005200990802     F*---
005300990802     F* COMPETENZE DETTAGLIO
005400990802     F*---
005500950307     FECCED30C  UF   E           K DISK
005600950307     FECCEDP0F  O  A E           K DISK
005700010829     F                                     RENAME(ECCEDP00:ECCEDS)
005800040220     F*---
005900990802     F* INFRAGRUPPO METRICUBI
006000990802     F*---
006100980325     FECFIM30C  UF   E           K DISK
006200980325     FECFIMP0F  O  A E           K DISK
006300980325     F                                     RENAME(ECFIMP00:ECFIMS00)
006400990802     F*---
006500990802     F* VOCI COMPETENZE TESTATA
006600990802     F*---
006700970903     FECECT01L  UF   E           K DISK
006800970903     FECECT00F  O  A E           K DISK
006900970903     F                                     RENAME(ECECT:ECECTS)
007000990802     F*---
007100990802     F* VOCI COMPETENZE DETTAGLIO
007200990802     F*---
007300970903     FECECD01L  UF   E           K DISK
007400970903     FECECD00F  O  A E           K DISK
007500970903     F                                     RENAME(ECECD:ECECDS)
007600000327     F*---
007700000327     F* EVENTI
007800000327     F*---
007900000327     FFNEVB01L  UF   E           K DISK
008000000327     FFNEVB00F  O  A E           K DISK
008100000327     F                                     RENAME(FNEVB000:FNEVBS)
008200131029     FFNEVB22L  UF   E           K DISK
008300000404     F*---
008400000404     F* GIACENZE
008500000404     F*---
008600050301     Ftigcp51l  UF   E           K DISK
008700050301     Ftigcp00f  O  A E           K DISK
008800050301     F                                     RENAME(tigcp000:tigcpS)
008900000404     F*---
009000000404     F* GIACENZE -ANAGRAFICHE MITTENTE-
009100000404     F*---
009200050301     Ftigap01l  UF   E           K DISK
009300050301     Ftigap00f  O  A E           K DISK
009400050301     F                                     RENAME(TIGAP000:TIGAPS)
009500000404     F*---
009600000404     F* GIACENZE -NOTE-
009700000404     F*---
009800050301     FTIGNP01L  UF   E           K DISK
009900050301     FTIGNP00F  O  A E           K DISK
010000050301     F                                     RENAME(TIGNP000:TIGNPS)
010100040303     F*---
010200040303     F* ESTENSIONE ALLA TESTATA BOLLE
010300040303     F*---
010400040531     FFIAR531C  UF   E           K DISK
010500040303     FFIAR5P0F  O  A E           K DISK
010600040303     F                                     RENAME(FIAR5P00:FIAR5S)
010700060919     F*---
010800060919     Ftitai01l  UF   E           K DISK
010900060919     Ftitai00F  O  A E           K DISK
011000060919     F                                     RENAME(titai000:titaiS)
011100081103     F*---
011200081103     Ffirab01l  UF   E           K DISK
011300081103     Ffirab00F  O  A E           K DISK
011400081103     F                                     RENAME(firab000:firabS)
011500061122     F*---
011600061122     Ffiapd01l  IF   E           K DISK
011700080312     Ftilad01l  IF   E           K DISK
011800000404     F*---
011900000327     F* TABELLE
012000000327     F*---
012100950309     FTABEL00F  UF   E           K DISK    USROPN
012200000327     F*---
012300000327     F* CONTRASSEGNI
012400000327     F*---
012500950309     FTNCSB03L  IF   E           K DISK
012600000327     F*---
012700000327     F* DANNI
012800000327     F*---
012900040715     F***FNDCT02L  IF   E           K DISK
013000040823     F*---
013100040823     Ffnblp01L  IF   E           K DISK    usropn
013200040823     Ffnarb01L  IF   E           K DISK    usropn
013300000327     F*---
013400000327     F* PRINTER FILE
013500000327     F*---
013600931105     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
013700931105     F                                     FORMLEN(66)
013800931105     F                                     FORMOFL(60)
013900940912     L*------------------------------------------------------------------------*
014000040823     d wlib            s             10    inz('FILTRA201 ')
014100040910     d wlibp           s             10    inz('FILTRAPRD ')
014200040823     d comman          s             80
014300040823     d lenght          s             15  5 inz(80)
014400061122     d trcapd          s                   like(apdtip) inz('A')
014500061122     d com04           s              4  0
014600150123     d PARDTF_1m       s                   like(PARDTF)
014700150123     d Data_ISO        s               d   datfmt(*iso)
014800131223      *------------------------------------------------------------------------*
014900131223
015000131223     D*------------------
015100131223     D* VARIABILI D WRK
015200131223     D*------------------
015300131223     D  wAAS           S                   like(tasAAS) inz
015400131223     D  wLNP           S                   like(tasLNP) inz
015500131223     D  wNRS           S                   like(tasNRS) inz
015600131223     D  wNSP           S                   like(tasNSP) inz
015700131223
015800131223     D*------------------
015900131223     D* LINKING A DEFINIZIONI ESTERNE
016000131223     D*------------------
016100131223
016200131223     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
016300131223     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
016400131223
016500940912     D*------------------------------------------------------------------------*
016600940912     D* SCHIERE
016700940912     D*------------------------------------------------------------------------*
016800040823     d cmd             s             60    dim(01) ctdata perrcd(1)
016900931105     D TES             S             66    DIM(15) CTDATA PERRCD(1)
017000080925     d tbl             s              2    dim(50) inz
017100950309     D* DS TABELLA $3 (DATE AVANZAMENTO TRASFERIMENTO BOLLE)
017200950309     D DS$3          E DS
017300080925     D DStb          E DS
017400950717     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
017500950717     D DS§DAT          DS
017600950717     D  DSSEC                  1      2  0
017700950717     D  DSANN                  3      4  0
017800950717     D  DSMES                  5      6  0
017900950717     D  DSGIO                  7      8  0
018000950717     D  DSAAS                  1      4  0
018100950717     D  DSMGS                  5      8  0
018200950717     D  DSDAT6                 3      8  0
018300950717     D  DSDAT8                 1      8  0
018400950717     D DS§TIM          DS
018500950717     D  DTORA                  1      2  0
018600950717     D  DTMIN                  3      4  0
018700950717     D  DTSCN                  5      6  0
018800950717     D  DTGIO                  7      8  0
018900950717     D  DTMES                  9     10  0
019000950717     D  DTSEC                 11     12  0
019100950717     D  DTANN                 13     14  0
019200950717     D  DTORA6                 1      6  0
019300950717     D  DTTIME                 1     14  0
019400940912     D*------------------------------------------------------------------------*
019500940912     D* INPUT
019600940912     D*------------------------------------------------------------------------*
019700941123     D                 DS
019800941123     D  DSAASI                 1      4  0
019900941123     D  DSMGSI                 5      8  0
020000950309     D  DSDATI                 1      8  0
020100941123     D                 DS
020200941123     D  DSAASF                 1      4  0
020300941123     D  DSMGSF                 5      8  0
020400950309     D  DSDATF                 1      8  0
020500020220     D                 DS
020600020220     D  DSAASFP                1      4  0
020700020220     D  DSMGSFP                5      8  0
020800020220     D  DSDATFP                1      8  0
020900960413     D*-------------------
021000960413     D* DATA SPEDIZIONE
021100960413     D*-------------------
021200960413     D                 DS
021300960413     D  TASAAS                 1      4  0
021400960413     D  TASMGS                 5      8  0
021500960413     D  TASDSP                 1      8  0
021600150123     D  TASANME                1      6  0
021700000327     D*------------------
021800000327     D* DS "XSRDA8" - CONTROLLA DATA (8)
021900000327     D*------------------
022000000327     D WLBDA8          DS                  INZ
022100000327     D  G08DAT                 1      8  0
022200000327     D  G08INV                 9     16  0
022300000327     D  G08ERR                17     17
022400000327     D  G08TGI                18     22  0
022500040715
022600040715     d Fidn12ds      e ds
022700040715     d  skKey                 26    305    dim(20)
022800040715     d  skAnn                306    325    dim(20)
022900040715     d  skDca                326    485    dim(20)
023000040715     d  skFca                486    545  0 dim(20)
023100040715     d  skDch                546    705  0 dim(20)
023200040715     d  skCch                706    745    dim(20)
023300040715
023400940912     D*-------------------
023500931115     D* PARAMETRI LANCIO PGM
023600940912     D*-------------------
023700000327     D PARAM           DS
023800950309     D  PARDTI                 1      8  0                                      DT.INI
023900950309     D  PARDTF                 9     16  0                                      DT.FIN
024000150123     D  PARDTFANME             9     14  0                                      DT.FIN AAAAMM
024100020220     D  PARDFP                17     24  0                                      DT.FIN PT
024200050706     D  PARcop                25     27                                         DT.FIN PT
024300940912     D*-------------------
024400940912     D* ARCHITETTURA
024500940912     D*-------------------
024600931115     D KPJBA         E DS
024700050705     D tnsb34ds      E DS
024800050705     D dtasflo       E DS
024900061121      *
025000061121     D*-------------------
025100061121     D* ANAGRAFICA
025200061121     D*-------------------
025300061121     d TIBS69ds      e ds                  inz
025400061121     d DS_cnaco      e ds                  inz  extname(CNACO00F)
025500061121     d DS_cnind      e ds                  inz  extname(CNIND00F)
025600061121     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
025700061121     d DS_fncls      e ds                  inz  extname(FNCLS00F)
025800061122     D*-------------------
025900061122     D* RIFERIMENTI ALFANUMERICI
026000061122     D*-------------------
026100061122     d DTA4A         e ds
026200061122     D*-------------------
026300061122     D* CODICE FISCALE
026400061122     D*-------------------
026500061122     d DTA4Q         e ds
026600120709
026700120709      * RCD DOC FIAR5
026800120709     d dAR5doc       e ds
026900120709
027000940912     C*------------------------------------------------------------------------*
027100940912     C*?MAIN LINE
027200940912     C*------------------------------------------------------------------------*
027300000000     C*
027400930129     C* ROUTINE INIZIALE
027500930129     C                   EXSR      RUTINZ
027600931105     C*
027700931105     C* STAMPA RIGA DI TESTATA
027800931105     C                   EXCEPT    TESTA
027900930601     C*
028000930601     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
028100930601     C     FINL00        IFEQ      '0'                                          --- 1 -->
028200000000     C*
028300990802     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA SU TITAS
028400950307     C                   EXSR      SETTAS
028500930128     C*
028600990802     C* CICLO FINO A FINE FILE TITAS OPPURE
028700950308     C* RICHIESTA CHIUSURA PGM OPPURE
028800950308     C* FINE PGM PER ERRORE
028900950308     C                   SETOFF                                       98
029000950307     C     FINTAS        DOWEQ     '0'                                          --- 2 -->
029100950308     C     *IN98         ANDEQ     '0'
029200950308     C     FINL00        ANDEQ     '0'
029300080214      * verifico   che non esistano ARB e BLP altrimenti non cancello
029400040823      * e leggo un'altro rekord
029500040823     C     Kbol          Chain     fnblp01l                           33
029600040922     C  N33              ADD       1             NUMblp                         *REC TRASF TITA7
029700040922     C  n33              EXCEPT    ERRblp
029800080214     C     kbol          Chain     fnarb01l                           34
029900080214     C  N34              ADD       1             NUMarb                         *REC TRASF TITA7
030000080214     C  n34              EXCEPT    ERRarb
030100080214     c                   if        not *in33 or not *in34
030200080214     c                   goto      altrorek
030300080214     c                   end
030400950307     C*
030500001019     C                   WRITE     TITASS00                             99      *SCRIVO TITASS
030600001019     C  N99              DELETE    TITASP00                             99      *CANCELLO TITASP
030700950307     C   99              EXCEPT    ERRTAS
030800950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
030900990802     C  N99              ADD       1             NUMTAS                         *REC TRASF TITAS
031000050705     c* se bolla ha immagine di laguna scrivo record sul file TILAD00F
031100050705     c                   if        not *in99
031200050705     c                   movel     tasflo        dtasflo
031300050705     c                   if        §floiml <> ' '
031400050705     c                   exsr      srlaguna
031500050705     c                   end
031600120709       //?se la bolla ha le immagini dei documenti firmati dal cliente da
031700120709       //?tornare al mittente scrivo nuovo record sul file TIJDD00F
031800120709      /free
031900120709        AR5trd = 'DOC';
032000120709        chain (TASaas:TASlnp:TASnrs:TASnsp:AR5trd) FIAR531C;
032100120709        IF  %found(FIAR531C);
032200120709          dAR5doc = AR5uni;
032300120709          IF  §AR5jfid <> *blanks;
032400120709            exsr srJdoc;
032500120709          ENDIF;
032600120709        ENDIF;
032700120709      /end-free
032800050705     c                   end
032900950307     C*
033000990802     C* SE ESISTE TITA7 TRASFERISCO ANCHE QUELLO
033100950307     C     TASSV3        IFNE      *BLANKS                                      --- 3 -->
033200990802     C                   EXSR      SETTA7                                       *PRIMO TITA7
033300950307     C     FINTA7        DOWEQ     '0'                                          --- 4 -->
033400990802     C                   WRITE     TITA7S00                             99      *SCRIVO TITA7S
033500001019     C  N99              DELETE    TITA730C                             99      *CANCELLO TITA730C
033600950307     C   99              EXCEPT    ERRTA7
033700950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
033800990802     C  N99              ADD       1             NUMTA7                         *REC TRASF TITA7
033900990802     C                   EXSR      LETTA7                                       *SUCCESSIVO TITA7
034000950307     C                   ENDDO                                                  <-- 4 ---
034100950307     C                   ENDIF                                                  <-- 3 ---
034200990802     C*
034300990802     C* TRASFERISCE TITAA - ANAGRAFICHE
034400990802     C                   EXSR      SETTAA                                       *PRIMO TITAA
034500990802DO  3C     FINTAA        DOWEQ     '0'
034600990802     C                   WRITE     TITAAS00                             99      *SCRIVO TITAAS
034700001019     C  N99              DELETE    TITAA30C                             99      *CANCELLO TITAA30C
034800990802     C   99              EXCEPT    ERRTAA
034900990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
035000990802     C  N99              ADD       1             NUMTAA                         *REC TRASF TITAA
035100990802     C                   EXSR      LETTAA                                       *SUCCESSIVO TITAA
035200990802E   3C                   ENDDO
035300061121     C* verifico se scritto i dati anagrafici del mittente altrimenti aggancio anagrafica
035400061121     c                   if        tasrsm = *blanks
035500061121     c                   exsr      recana
035600061121     c                   endif
035700990802     C*
035800990802     C* TRASFERISCE TITAT - SEGNACOLLI
035900990802     C                   EXSR      SETTAT                                       *PRIMO TITAT
036000990802DO  3C     FINTAT        DOWEQ     '0'
036100990802     C                   WRITE     TITATS00                             99      *SCRIVO TITATS
036200001019     C  N99              DELETE    TITAT30C                             99      *CANCELLO TITAT30C
036300990802     C   99              EXCEPT    ERRTAT
036400990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
036500990802     C  N99              ADD       1             NUMTAT                         *REC TRASF TITAT
036600990802     C                   EXSR      LETTAT                                       *SUCCESSIVO TITAT
036700990802E   3C                   ENDDO
036800990802     C*
036900990802     C* TRASFERISCE TITA4 - RIFERIMENTI
037000990802     C                   EXSR      SETTA4                                       *PRIMO TITA4
037100990802DO  3C     FINTA4        DOWEQ     '0'
037200990802     C                   WRITE     TITA4S00                             99      *SCRIVO TITA4S
037300001019     C  N99              DELETE    TITA430C                             99      *CANCELLO TITA430C
037400990802     C   99              EXCEPT    ERRTA4
037500990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
037600990802     C  N99              ADD       1             NUMTA4                         *REC TRASF TITA4
037700990802     C                   EXSR      LETTA4                                       *SUCCESSIVO TITA4
037800990802E   3C                   ENDDO
037900010903     C*
038000010903     C* TRASFERISCE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
038100010903     C                   EXSR      SETTAH                                       *PRIMO TITAH
038200010903DO  3C     FINTAH        DOWEQ     '0'
038300010903     C                   WRITE     TITAHS00                             99      *SCRIVO TITAHS
038400010903     C  N99              DELETE    TITAH30C                             99      *CANCELLO TITAH30C
038500010903     C   99              EXCEPT    ERRTAH
038600010903     C   99              MOVEL     '1'           FINL00                         *FINE PGM
038700010903     C  N99              ADD       1             NUMTAH                         *REC TRASF TITAH
038800010903     C                   EXSR      LETTAH                                       *SUCCESSIVO TITAH
038900010903E   3C                   ENDDO
039000950307     C*
039100950307     C* SE ESISTE ECCET LO TRASFERISCE
039200971008     C                   Z-ADD     999           MINNBC                         MIMIMO BLOCCO
039300950307     C                   Z-ADD     TASAAS        CETAAS                         ANNO SPEDIZIONE
039400950307     C                   Z-ADD     TASLNP        CETLNP                         LINEA DI PARTENZA
039500950307     C                   Z-ADD     TASNRS        CETNRS                         NUMERO SERIE
039600950307     C                   Z-ADD     TASNSP        CETNSP                         NUMERO SPEDIZIONE
039700950307     C                   MOVEL     TASTBL        CETTBL                         TIPO BOLLA
039800950307     C     KEYCET        CHAIN     ECCET30C                           99
039900950307     C     *IN99         IFEQ      *OFF                                         --- 3 --> *TROVATO
040000970903     C*
040100970903     C* MEMORIZZA IL NUMERO BLOCCO COMPETENZE TESTATA PIU' PICCOLO
040200971008IF  4C     CETNBC        IFGT      *ZEROS
040300971008IF  5C     CETNBC        IFLT      MINNBC
040400970903     C                   Z-ADD     CETNBC        MINNBC
040500971008E   5C                   ENDIF
040600971008E   4C                   ENDIF
040700970903     C*
040800950309     C                   WRITE     ECCETS                               99      *SCRIVO ECCETS
040900001019     C  N99              DELETE    ECCET30C                             99      *CANCELLO ECCET30C
041000950307     C   99              EXCEPT    ERRCET
041100950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
041200950308     C  N99              ADD       1             NUMCET                         *REC TRASF ECCET
041300950307     C*
041400950307     C* SE ESISTE ECCED TRASFERISCO ANCHE QUELLO
041500950307     C     CETDET        IFEQ      'S'                                          --- 4 -->
041600950307     C                   EXSR      SETCED                                       *PRIMO ECCED
041700950307     C     FINCED        DOWEQ     '0'                                          --- 5 -->
041800950309     C                   WRITE     ECCEDS                               99      *SCRIVO ECCEDS
041900001020     C  N99              DELETE    ECCED30C                             99      *CANCELLO ECCED30C
042000950307     C   99              EXCEPT    ERRCED
042100950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
042200950308     C  N99              ADD       1             NUMCED                         *REC TRASF ECCED
042300950307     C                   EXSR      LETCED                                       *SUCCESSIVO ECCED
042400950307     C                   ENDDO                                                  <-- 5 ---
042500950307     C                   ENDIF                                                  <-- 4 ---
042600950307     C                   ENDIF                                                  <-- 3 ---
042700980325     C*
042800980325     C* SE ESISTE ECFIM LO TRASFERISCE
042900980325     C                   Z-ADD     TASAAS        FIMAAS                         ANNO SPEDIZIONE
043000980325     C                   Z-ADD     TASLNP        FIMLNP                         LINEA DI PARTENZA
043100980325     C                   Z-ADD     TASNRS        FIMNRS                         NUMERO SERIE
043200980325     C                   Z-ADD     TASNSP        FIMNSP                         NUMERO SPEDIZIONE
043300980325     C                   MOVEL     TASTBL        FIMTBL                         TIPO BOLLA
043400001019     C     KEYFIM        CHAIN     ECFIM30C                           99
043500980325     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
043600980325     C                   WRITE     ECFIMS00                             99      *SCRIVE
043700001019     C  N99              DELETE    ECFIM30C                             99      *CANCELLA
043800980325     C   99              EXCEPT    ERRFIM
043900980325     C   99              MOVEL     '1'           FINL00                         *FINE PGM
044000980325     C  N99              ADD       1             NUMFIM                         *REC TRASF ECCET
044100011127     C     KEYFIM        READE     ECFIM30C                               99
044200980325     C                   ENDDO                                                  <-- 3 ---
044300000327     C*
044400000327     C* SE ESISTE FNEVB LO TRASFERISCE
044500000327     C                   Z-ADD     TASAAS        EVBAAS                         ANNO SPEDIZIONE
044600000327     C                   Z-ADD     TASLNP        EVBLNP                         LINEA DI PARTENZA
044700000327     C                   Z-ADD     TASNRS        EVBNRS                         NUMERO SERIE
044800000327     C                   Z-ADD     TASNSP        EVBNSP                         NUMERO SPEDIZIONE
044900001019     C     KEYEVB        CHAIN     FNEVB01L                           99
045000000327     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
045100000327     C                   WRITE     FNEVBS                               99      *SCRIVE
045200001019     C  N99              DELETE    FNEVB01L                             99      *CANCELLA
045300131029     c                   if        not *in99
045400131029     C     Kevb2f        chain     FNEVB22L
045500131029     c                   if        %found(fnevb22l)
045600131029     c                   delete    fnevb22l
045700131029     c                   endif
045800131029     c                   endif
045900000327     C   99              EXCEPT    ERREVB
046000000327     C   99              MOVEL     '1'           FINL00                         *FINE PGM
046100000327     C  N99              ADD       1             NUMEVB                         *REC TRASF FNEVB
046200000327     C     KEYEVB        READE     FNEVB01L                               99
046300000327     C                   ENDDO                                                  <-- 3 ---
046400000404     C*
046500050301     C* SE ESISTE TIGCP LO TRASFERISCE
046600001018     C     TASFGC        IFEQ      'S'
046700000404     C                   Z-ADD     TASAAS        KCPAAS                         ANNO SPEDIZIONE
046800000404     C                   Z-ADD     TASLNP        KCPLNP                         LINEA DI PARTENZA
046900000404     C                   Z-ADD     TASNRS        KCPNRS                         NUMERO SERIE
047000000404     C                   Z-ADD     TASNSP        KCPNSP                         NUMERO SPEDIZIONE
047100050301     C     KE2GCP        CHAIN     TIGCP51L                           99
047200001018     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
047300050303     c     gcpsede       ifeq      'S'
047400050301     C                   WRITE     TIGCPS                               99      *SCRIVE
047500050301     C  N99              DELETE    TIGCP51L                             99      *CANCELLA
047600000404     C   99              EXCEPT    ERRGCP
047700000404     C   99              MOVEL     '1'           FINL00                         *FINE PGM
047800000404     C  N99              ADD       1             NUMGCP                         *REC TRASF FNEVB
047900000404     C*
048000050301     C* SE ESISTE TIGAP LO TRASFERISCE
048100000404     C                   Z-ADD     GCPAAS        KAPAAS                         ANNO SPEDIZIONE
048200000404     C                   Z-ADD     GCPLNP        KAPLNP                         LINEA DI PARTENZA
048300000404     C                   Z-ADD     GCPNRS        KAPNRS                         NUMERO SERIE
048400000404     C                   Z-ADD     GCPNSP        KAPNSP                         NUMERO SPEDIZIONE
048500050301     C     KEYGAP        CHAIN     TIGAP01L                           99
048600001018     C     *IN99         DOWEQ     *OFF
048700050301     C                   WRITE     TIGAPS                               99      *SCRIVE
048800050301     C  N99              DELETE    TIGAP01L                             99      *CANCELLA
048900001017     C   99              EXCEPT    ERRGAP
049000001017     C   99              MOVEL     '1'           FINL00                         *FINE PGM
049100001017     C  N99              ADD       1             NUMGAP                         *REC TRASF FNEVB
049200050301     C     KEYGAP        READE     TIGAP01L                               99
049300001018     C                   ENDDO
049400000404     C*
049500050301     C* SE ESISTE TIGNP LO TRASFERISCE
049600000404     C                   Z-ADD     GCPAGC        KNPAGC                         ANNO APERTURA
049700000404     C                   Z-ADD     GCPFGC        KNPFGC                         FILIALE APERTURA
049800000404     C                   Z-ADD     GCPNGC        KNPNGC                         NUMERO GIACENZA
049900050301     C     KEYGNP        CHAIN     TIGNP01L                           99
050000001018     C     *IN99         DOWEQ     *OFF
050100050301     C                   WRITE     TIGNPS                               99      *SCRIVE
050200050301     C  N99              DELETE    TIGNP01L                             99      *CANCELLA
050300001017     C   99              EXCEPT    ERRGNP
050400001017     C   99              MOVEL     '1'           FINL00                         *FINE PGM
050500001017     C  N99              ADD       1             NUMGNP                         *REC TRASF FNEVB
050600050301     C     KEYGNP        READE     TIGNP01L                               99
050700001018     C                   ENDDO
050800050303     c                   else
050900050303     C                   EXCEPT    ERRGCP1
051000060707     C*                  MOVEL     '1'           FINL00                         *FINE PGM
051100050303     C                   ENDIF
051200000404     C*
051300000404     C* LETTURA SUCCESSIVA GIACENZE
051400050301     C     KE2GCP        READE     TIGCP51L                               99
051500001018     C                   ENDDO
051600001018     C                   ENDIF
051700040303     C*
051800040303     C* SE ESISTE FIAR5 LO TRASFERISCE
051900040303     C                   Z-ADD     TASAAS        AR5AAS                         ANNO SPEDIZIONE
052000040303     C                   Z-ADD     TASLNP        AR5LNP                         LINEA DI PARTENZA
052100040303     C                   Z-ADD     TASNRS        AR5NRS                         NUMERO SERIE
052200040303     C                   Z-ADD     TASNSP        AR5NSP                         NUMERO SPEDIZIONE
052300040531     C     KEYAR5        CHAIN     FIAR531C                           99
052400040303     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
052500040303     C                   WRITE     FIAR5S                               99      *SCRIVE
052600040531     C  N99              DELETE    FIAR531C                             99      *CANCELLA
052700040303     C   99              EXCEPT    ERRAR5
052800040303     C   99              MOVEL     '1'           FINL00                         *FINE PGM
052900040303     C  N99              ADD       1             NUMAR5                         *REC TRASF FNEVB
053000040531     C     KEYAR5        READE     FIAR531C                               99
053100040303     C                   ENDDO                                                  <-- 3 ---
053200060919     C*
053300060919     C* SE ESISTE TITAI LO TRASFERISCE
053400060919     C                   Z-ADD     TASAAS        taiAAS                         ANNO SPEDIZIONE
053500060919     C                   Z-ADD     TASLNP        taiLNP                         LINEA DI PARTENZA
053600060919     C                   Z-ADD     TASNRS        taiNRS                         NUMERO SERIE
053700060919     C                   Z-ADD     TASNSP        taiNSP                         NUMERO SPEDIZIONE
053800060919     C                   movel     TASTBL        taiTBL                         NUMERO SPEDIZIONE
053900060919     C     KEYtai        CHAIN     titai01l                           99
054000060919     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
054100060919     C                   WRITE     titaiS                               99      *SCRIVE
054200060919     C  N99              DELETE    titai01l                             99      *CANCELLA
054300060919     C   99              EXCEPT    ERRtai
054400060919     C   99              MOVEL     '1'           FINL00                         *FINE PGM
054500060919     C  N99              ADD       1             NUMtai                         *REC TRASF FNEVB
054600060919     C     KEYtai        READE     titai01l                               99
054700060919     C                   ENDDO                                                  <-- 3 ---
054800081103     C* SE ESISTE FIRAB LO TRASFERISCE
054900081103     C                   Z-ADD     TASAAS        rabAAS                         ANNO SPEDIZIONE
055000081103     C                   Z-ADD     TASLNP        rabLNP                         LINEA DI PARTENZA
055100081103     C                   Z-ADD     TASNRS        rabNRS                         NUMERO SERIE
055200081103     C                   Z-ADD     TASNSP        rabNSP                         NUMERO SPEDIZIONE
055300081103     C     KEYrab        CHAIN     firab01l                           99
055400081103     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
055500081103     C                   WRITE     firabS                               99      *SCRIVE
055600081103     C  N99              DELETE    firab01l                             99      *CANCELLA
055700081103     C   99              EXCEPT    ERRrab
055800081103     C   99              MOVEL     '1'           FINL00                         *FINE PGM
055900081103     C  N99              ADD       1             NUMrab                         *REC TRASF FNEVB
056000081103     C     KEYrab        READE     firab01l                               99
056100081103     C                   ENDDO                                                  <-- 3 ---
056200061214      * decodifico alcuni dati per il nuovo storico  e lo scrivo se non bolla Poste
056300061214     c                   If        tastsp <> 'P'
056400080926     c* se bolla di recupero non scrivo storico
056500080926     c                   z-add     1             conta
056600080926     c     tastbl        lookup    tbl(conta)                             44
056700080926     c                   if        not *in44
056800061122     c                   EXSR      wrttassto
056900061214     c                   endif
057000080926     c                   endif
057100061122
057200000000     C*
057300980325     C* LETTURA SUCCESSIVA BOLLE
057400040823     c     altrorek      tag
057500040823
057600060707     c                   if        finl00 = '0'
057700990802     C                   EXSR      LETTAS                                       *SUCCESSIVO TITAS
057800060707     c                   end
057900950308     C                   SHTDN                                        98        *CONTR.CHIUSURA PGM
058000930128     C*
058100950307     C                   ENDDO                                                  <-- 2 ---
058200950307     C                   ENDIF                                                  <-- 1 ---
058300970903     C*
058400970903     C* PULISCE I CODICI COMPETENZA -TESTATA E DETTAGLIO- CON NUMERO BLOCCO
058500970903     C* MINORE DI QUELLO PIU' PICCOLO CON CUI SONO STATE CALCOLATE LE BOLLE
058600971008IF  1C     MINNBC        IFNE      999
058700970903     C     *LOVAL        SETLL     ECECT01L
058800970903     C                   READ      ECECT01L                               97
058900971008DO  2C     *IN97         DOWEQ     *OFF
059000971008IF  3C     ECTATB        IFEQ      *BLANKS
059100971008IF  4C     ECTNBC        IFLT      MINNBC
059200970903     C                   MOVEL     'A'           ECTATB
059300970903     C                   MOVEL     *BLANKS       ECTFTR
059400970903     C                   EXCEPT    AGGECT                                       *AGGIORNAMENTO CAMPI
059500970903     C                   WRITE     ECECTS                               97      *SCRIVO ECECTS
059600970903     C   97              EXCEPT    ERRECT                                       *SCRIVE ERRORE
059700970903     C   97              MOVEL     '1'           FINL00                         *FINE PGM
059800970903     C  N97              ADD       1             NUMECT                         *REC TRASF ECECT
059900971008E   4C                   ENDIF
060000971008E   3C                   ENDIF
060100970903     C                   READ      ECECT01L                               97
060200971008E   2C                   ENDDO
060300950309     C*
060400970903     C     *LOVAL        SETLL     ECECD01L
060500970903     C                   READ      ECECD01L                               97
060600971008DO  2C     *IN97         DOWEQ     *OFF
060700971008IF  3C     ECDATB        IFEQ      *BLANKS
060800971008IF  4C     ECDNBC        IFLT      MINNBC
060900970903     C                   MOVEL     'A'           ECDATB
061000970903     C                   MOVEL     *BLANKS       ECDFTR
061100970903     C                   EXCEPT    AGGECD                                       *AGGIORNAMENTO CAMPI
061200970903     C                   WRITE     ECECDS                               97      *SCRIVO ECECDS
061300970903     C   97              EXCEPT    ERRECD                                       *SCRIVE ERRORE
061400970903     C   97              MOVEL     '1'           FINL00                         *FINE PGM
061500970903     C  N97              ADD       1             NUMECD                         *REC TRASF ECECD
061600971008E   4C                   ENDIF
061700971008E   3C                   ENDIF
061800970903     C                   READ      ECECD01L                               97
061900971008E   2C                   ENDDO
062000971008E   1C                   ENDIF
062100970903     C*
062200950309     C* AGGIORNA LA TABELLA CON L'ULTIMA DATA DI TRASFERIM. (SOLO SE TUTTO OK)
062300950309     C     *IN98         IFEQ      '0'
062400950309     C     FINL00        ANDEQ     '0'
062500950309     C                   EXSR      AGGTAB
062600950309     C                   ENDIF
062700931105     C*
062800931105     C* STAMPA RIGA DI FINE STAMPA
062900950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
063000931105     C                   EXCEPT    STAFIN
063100050705     c* chiudo il pgm di archiviazione Laguna
063200050705     C                   CLEAR                   Tnsb34ds
063300050705     C                   MOVEL     'C'           sb34tla
063400050705     C                   CALL      'TNSB34R'
063500050705     C                   PARM                    TNSB34ds
063600000000     C*
063700000000     C                   SETON                                        LR
063800940912     C*------------------------------------------------------------------------*
063900990802     C*? SETTAS - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAS
064000940912     C*------------------------------------------------------------------------*
064100950307     C     SETTAS        BEGSR
064200931115     C*
064300931115     C* AZZERO CONTATORE RECORD LETTI
064400950307     C                   MOVEL     '0'           FINTAS            1
064500930208     C*
064600930316     C* POSIZIONAMENTO E PRIMA LETTURA
064700941123     C                   Z-ADD     PARDTI        DSDATI
064800941123     C                   Z-ADD     PARDTF        DSDATF
064900020220     C                   Z-ADD     PARDFP        DSDATFP
065000941123     C                   Z-ADD     DSAASI        TASAAS
065100941123     C                   Z-ADD     DSMGSI        TASMGS
065200990802     C     KEYTAS        SETLL     TITAS38C                           99        *FINE-ARCHIV
065300930625     C*
065400930625     C     *IN99         IFEQ      '1'
065500950307     C                   MOVEL     '1'           FINTAS
065600930625     C                   ELSE
065700950307     C                   EXSR      LETTAS
065800001020     C                   ENDIF
065900930208     C*
066000930208     C                   ENDSR
066100940912     C*------------------------------------------------------------------------*
066200990802     C*? LETTAS - LETTURA PROSSIMO RECORD TITAS
066300940912     C*------------------------------------------------------------------------*
066400950307     C     LETTAS        BEGSR
066500930625     C*
066600930625     C* LEGGO FINO A:
066700930625     C*     - FINE FILE O SUPERATO LIMITE FINALE
066800930625     C*     - TROVATO RECORD VALIDO
066900930625     C                   MOVEL     'N'           WRECOK
067000950307     C     FINTAS        DOWEQ     '0'                                          --- 1 -->
067100930625     C     WRECOK        ANDEQ     'N'
067200061121      * pulisco lo storico
067300061121     c                   clear                   titassto
067400930208     C*
067500930316     C* LETTURE SUCCESSIVE ALLA PRIMA
067600990802     C                   READ      TITAS38C                               99
067700960413     C*
067800960413     C     *IN99         IFEQ      '1'                                          *FINE FILE
067900020220     C     TASDSP        ORGT      DSDATF                                       *SUPERATA DATA FINE
068000950307     C                   MOVEL     '1'           FINTAS
068100930625     C                   ELSE
068200950309     C                   EXSR      CHKTAS                                       *CONTR.VALIDITA REC.
068300001019     C                   ENDIF
068400930625     C*
068500001019     C                   ENDDO                                                  <-- 1 ---
068600930208     C*
068700930208     C                   ENDSR
068800950307     C*------------------------------------------------------------------------*
068900990802     C*? SETTA7 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA7
069000950307     C*------------------------------------------------------------------------*
069100950307     C     SETTA7        BEGSR
069200950307     C*
069300950307     C* AZZERO CONTATORE RECORD LETTI
069400950307     C                   MOVEL     '0'           FINTA7            1
069500950307     C*
069600950307     C* POSIZIONAMENTO E PRIMA LETTURA
069700950307     C                   Z-ADD     TASAAS        TA7AAS
069800950307     C                   Z-ADD     TASLNP        TA7LNP
069900950307     C                   Z-ADD     TASNRS        TA7NRS
070000950307     C                   Z-ADD     TASNSP        TA7NSP
070100950307     C                   MOVEL     TASTBL        TA7TBL
070200001019     C*
070300001019     C     KEYTA7        CHAIN     TITA730C                           99        *FINE-ARCHIV
070400950307     C     *IN99         IFEQ      '1'
070500950307     C                   MOVEL     '1'           FINTA7
070600001019     C                   ENDIF
070700950307     C*
070800950307     C                   ENDSR
070900950307     C*------------------------------------------------------------------------*
071000990802     C*? LETTA7 - LETTURA PROSSIMO RECORD TITA7
071100950307     C*------------------------------------------------------------------------*
071200001019     C     LETTA7        BEGSR
071300001020     C*
071400001020     C* LETTURE SUCCESSIVE ALLA PRIMA
071500010124     C                   READ      TITA730C                               99
071600950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
071700010124     C     TA7AAS        ORNE      TASAAS
071800010124     C     TA7LNP        ORNE      TASLNP
071900010124     C     TA7NRS        ORNE      TASNRS
072000010124     C     TA7NSP        ORNE      TASNSP
072100010124     C     TA7TBL        ORNE      TASTBL
072200950307     C                   MOVEL     '1'           FINTA7
072300001019     C                   ENDIF
072400950307     C*
072500950307     C                   ENDSR
072600990802     C*------------------------------------------------------------------------*
072700990802     C*? SETTAA - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAA - ANAGRAFICHE
072800990802     C*------------------------------------------------------------------------*
072900990802     C     SETTAA        BEGSR
073000990802     C*
073100990802     C* AZZERO CONTATORE RECORD LETTI
073200990802     C                   MOVEL     '0'           FINTAA            1
073300990802     C*
073400990802     C* POSIZIONAMENTO E PRIMA LETTURA
073500990802     C                   Z-ADD     TASAAS        TAAAAS
073600990802     C                   Z-ADD     TASLNP        TAALNP
073700990802     C                   Z-ADD     TASNRS        TAANRS
073800990802     C                   Z-ADD     TASNSP        TAANSP
073900001019     C     KEYTAA        CHAIN     TITAA30C                           99        *FINE-ARCHIV
074000990802     C     *IN99         IFEQ      '1'
074100990802     C                   MOVEL     '1'           FINTAA
074200061121      * recupero dati anagrafici del mittente
074300061121     c                   else
074400061121     c                   exsr      rectaa
074500001019     C                   ENDIF
074600990802     C*
074700990802     C                   ENDSR
074800990802     C*------------------------------------------------------------------------*
074900990802     C*? LETTAA - LETTURA PROSSIMO RECORD TITAA - ANAGRAFICHE
075000990802     C*------------------------------------------------------------------------*
075100990802     C     LETTAA        BEGSR
075200010124     C*
075300010124     C                   READ      TITAA30C                               99
075400990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
075500010130     C     TAAAAS        ORNE      TASAAS
075600010130     C     TAALNP        ORNE      TASLNP
075700010130     C     TAANRS        ORNE      TASNRS
075800010130     C     TAANSP        ORNE      TASNSP
075900990802     C                   MOVEL     '1'           FINTAA
076000001019     C                   ENDIF
076100061121      * se letto
076200061123     c                   if        fintaa = '0'
076300061121     c                   exsr      rectaa
076400061121     c                   endif
076500990802     C*
076600990802     C                   ENDSR
076700061121     C*------------------------------------------------------------------------*
076800061121     C*? RECTAA - RECUPERO DATI ANAGRAFICI MITTENTE
076900061121     C*------------------------------------------------------------------------*
077000061121     C     RECTAA        BEGSR
077100061121      * dati anagrafici mittente
077200061121     c                   IF        taatrc = 'M'
077300061121     c                   eval      tasrsm = taarsc
077400061121     c                   eval      tasinm = taaind
077500061121     c                   eval      tascam = taacap
077600061121     c                   eval      taslom = taaloc
077700061121     c                   eval      tasprm = taaprv
077800061121     c                   eval      tasnzm = taanaz
077900061122      * iva  per i franchi non coidficati
078000061122     c                   if        %subst(tastbl:1:1) = 'F'
078100061122     c                   eval      tasiva = taacpi
078200061122     c                   endif
078300061122
078400061121     c                   endif
078500061121
078600061121     c                   endsr
078700061121     C*------------------------------------------------------------------------*
078800061121     C*? RECANA - RECUPERO DA ANAGRAFICO CLIENTE
078900061121     C*------------------------------------------------------------------------*
079000061121     C     RECANA        BEGSR
079100061121     C                   clear                   ds_CNACO
079200061121     C                   clear                   ds_CNIND
079300061121     C                   clear                   ds_CNCLP
079400061121     C                   clear                   ds_FNCLS
079500061121     c                   z-add     TASccm        I69kac
079600061121     c                   z-add     TASccm        I69kin
079700061121     c                   call      'TIBS69R'
079800061121     c                   parm                    TIBS69ds
079900061121     c                   parm                    ds_CNACO
080000061121     c                   parm                    ds_CNIND
080100061121     c                   parm                    ds_CNCLP
080200061121     c                   parm                    ds_FNCLS
080300061122if  3c                   if        O69err <> *on
080400061122     c                   movel     ACOrag        TASrsm
080500061122     c                   movel     INDvia        TASinm
080600061122     c                   movel     INDcap        TAScam
080700061122     c                   movel     INDcit        TASlom
080800061122     c                   movel     INDprv        TASprm
080900061122     c                   movel     INDsta        TASnzm
081000061122e   3c                   endif
081100061121
081200061121     c                   endsr
081300061122     C*------------------------------------------------------------------------*
081400061122     C*? RECIVA - RECUPERO DA ANAGRAFICO CLIENTE IVA E/O CODICE FISCALE
081500061122     C*------------------------------------------------------------------------*
081600061122     C     RECIVA        BEGSR
081700061122     C                   clear                   ds_CNACO
081800061122     C                   clear                   ds_CNIND
081900061122     C                   clear                   ds_CNCLP
082000061122     C                   clear                   ds_FNCLS
082100061122     c                   z-add     TASKSC        I69kin
082200061122     c                   call      'TIBS69R'
082300061122     c                   parm                    TIBS69ds
082400061122     c                   parm                    ds_CNACO
082500061122     c                   parm                    ds_CNIND
082600061122     c                   parm                    ds_CNCLP
082700061122     c                   parm                    ds_FNCLS
082800061122
082900061122     c                   endsr
083000990802     C*------------------------------------------------------------------------*
083100990802     C*? SETTAT - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAT - SEGNACOLLI
083200990802     C*------------------------------------------------------------------------*
083300990802     C     SETTAT        BEGSR
083400990802     C*
083500990802     C* AZZERO CONTATORE RECORD LETTI
083600990802     C                   MOVEL     '0'           FINTAT            1
083700990802     C*
083800990802     C* POSIZIONAMENTO E PRIMA LETTURA
083900990802     C                   Z-ADD     TASAAS        TATAAS
084000990802     C                   Z-ADD     TASLNP        TATLNP
084100990802     C                   Z-ADD     TASNRS        TATNRS
084200990802     C                   Z-ADD     TASNSP        TATNSP
084300001019     C     KEYTAT        CHAIN     TITAT30C                           99        *FINE-ARCHIV
084400990802     C     *IN99         IFEQ      '1'
084500990802     C                   MOVEL     '1'           FINTAT
084600001019     C                   ENDIF
084700990802     C*
084800990802     C                   ENDSR
084900990802     C*------------------------------------------------------------------------*
085000990802     C*? LETTAT - LETTURA PROSSIMO RECORD TITAT - SEGNACOLLI
085100990802     C*------------------------------------------------------------------------*
085200990802     C     LETTAT        BEGSR
085300010124     C*
085400010124     C                   READ      TITAT30C                               99
085500990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
085600010124     C     TATAAS        ORNE      TASAAS
085700010124     C     TATLNP        ORNE      TASLNP
085800010124     C     TATNRS        ORNE      TASNRS
085900010124     C     TATNSP        ORNE      TASNSP
086000990802     C                   MOVEL     '1'           FINTAT
086100001019     C                   ENDIF
086200990802     C*
086300990802     C                   ENDSR
086400990802     C*------------------------------------------------------------------------*
086500990802     C*? SETTA4 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA4 - RIFERIMENTI
086600990802     C*------------------------------------------------------------------------*
086700990802     C     SETTA4        BEGSR
086800990802     C*
086900990802     C* AZZERO CONTATORE RECORD LETTI
087000990802     C                   MOVEL     '0'           FINTA4            1
087100990802     C*
087200990802     C* POSIZIONAMENTO E PRIMA LETTURA
087300990802     C                   Z-ADD     TASAAS        TA4AAS
087400990802     C                   Z-ADD     TASLNP        TA4LNP
087500990802     C                   Z-ADD     TASNRS        TA4NRS
087600990802     C                   Z-ADD     TASNSP        TA4NSP
087700001019     C     KEYTA4        CHAIN     TITA430C                           99        *FINE-ARCHIV
087800990802     C     *IN99         IFEQ      '1'
087900990802     C                   MOVEL     '1'           FINTA4
088000061122      * recupero riferimento mittente alfanumerico
088100061122     c                   else
088200061122     c                   exsr      recta4
088300001019     C                   ENDIF
088400990802     C*
088500990802     C                   ENDSR
088600990802     C*------------------------------------------------------------------------*
088700990802     C*? LETTA4 - LETTURA PROSSIMO RECORD TITA4 - RIFERIMENTI
088800990802     C*------------------------------------------------------------------------*
088900990802     C     LETTA4        BEGSR
089000010124     C*
089100010124     C                   READ      TITA430C                               99
089200990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
089300010124     C     TA4AAS        ORNE      TASAAS
089400010124     C     TA4LNP        ORNE      TASLNP
089500010124     C     TA4NRS        ORNE      TASNRS
089600010124     C     TA4NSP        ORNE      TASNSP
089700990802     C                   MOVEL     '1'           FINTA4
089800001019     C                   ENDIF
089900061122      * se letto
090000061123     c                   if        finta4 = '0'
090100061122     c                   exsr      recta4
090200061122     c                   endif
090300990802     C*
090400990802     C                   ENDSR
090500061122     C*------------------------------------------------------------------------*
090600061122     C*? RECTA4 - RECUPERO RIFERIMENTO MITTENTE ALFANUMERICO / CODICE FISCALE
090700061122     C*------------------------------------------------------------------------*
090800061122     C     RECTA4        BEGSR
090900061122      * riferimento mittente alfanumerico
091000061122     c                   IF        ta4trc = 'A'
091100061122     c                   movel     ta4not        dta4a
091200061122     c                   movel     §ta4aRMA      Tasrma
091300061122     c                   endif
091400061122      * CODICE FISCALE
091500061122     c                   IF        ta4trc = 'Q'
091600061122     c                   movel     ta4not        dta4q
091700061122      * VERIFICO SE FRANCO O ASSEGNATO
091800061122     c                   If        %subst(tastbl:1:1) = 'F'
091900061122     c                   movel     §TA4QCFM      Tascdf
092000061122     c                   endif
092100061122     c                   If        %subst(tastbl:1:1) = 'A'
092200061122     c                   movel     §TA4QCFD      Tascdf
092300061122     c                   endif
092400061122     c                   endif
092500061122
092600061122     c                   endsr
092700010903     C*------------------------------------------------------------------------*
092800010903     C*? SETTAH - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
092900010903     C*------------------------------------------------------------------------*
093000010903     C     SETTAH        BEGSR
093100010903     C*
093200010903     C* AZZERO CONTATORE RECORD LETTI
093300010903     C                   MOVEL     '0'           FINTAH            1
093400010903     C*
093500010903     C* POSIZIONAMENTO E PRIMA LETTURA
093600010903     C                   Z-ADD     TASAAS        TAHAAS
093700010903     C                   Z-ADD     TASLNP        TAHLNP
093800010903     C                   Z-ADD     TASNRS        TAHNRS
093900010903     C                   Z-ADD     TASNSP        TAHNSP
094000010903     C     KEYTAH        CHAIN     TITAH30C                           99        *FINE-ARCHIV
094100010903     C     *IN99         IFEQ      '1'
094200010903     C                   MOVEL     '1'           FINTAH
094300010903     C                   ENDIF
094400010903     C*
094500010903     C                   ENDSR
094600010903     C*------------------------------------------------------------------------*
094700010903     C*? LETTAH - LETTURA PROSSIMO RECORD TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
094800010903     C*------------------------------------------------------------------------*
094900010903     C     LETTAH        BEGSR
095000010903     C*
095100010903     C                   READ      TITAH30C                               99
095200010903     C     *IN99         IFEQ      '1'                                          *FINE FILE
095300010903     C     TAHAAS        ORNE      TASAAS
095400010903     C     TAHLNP        ORNE      TASLNP
095500010903     C     TAHNRS        ORNE      TASNRS
095600010903     C     TAHNSP        ORNE      TASNSP
095700010903     C                   MOVEL     '1'           FINTAH
095800010903     C                   ENDIF
095900010903     C*
096000010903     C                   ENDSR
096100950307     C*------------------------------------------------------------------------*
096200950307     C*? SETCED - POSIZIONAMENTO E PRIMA LETTURA DEL FILE ECCED
096300950307     C*------------------------------------------------------------------------*
096400950307     C     SETCED        BEGSR
096500950307     C*
096600950307     C* AZZERO CONTATORE RECORD LETTI
096700950307     C                   MOVEL     '0'           FINCED            1
096800950307     C*
096900950307     C* POSIZIONAMENTO E PRIMA LETTURA
097000950307     C                   Z-ADD     TASAAS        CEDAAS
097100950307     C                   Z-ADD     TASLNP        CEDLNP
097200950307     C                   Z-ADD     TASNRS        CEDNRS
097300950307     C                   Z-ADD     TASNSP        CEDNSP
097400950307     C                   MOVEL     TASTBL        CEDTBL
097500001019     C     KEYCED        CHAIN     ECCED30C                           99        *FINE-ARCHIV
097600950307     C     *IN99         IFEQ      '1'
097700950307     C                   MOVEL     '1'           FINCED
097800001019     C                   ENDIF
097900950307     C*
098000950307     C                   ENDSR
098100950307     C*------------------------------------------------------------------------*
098200950307     C*? LETCED - LETTURA PROSSIMO RECORD ECCED
098300950307     C*------------------------------------------------------------------------*
098400950307     C     LETCED        BEGSR
098500950307     C*
098600010124     C                   READ      ECCED30C                               99
098700950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
098800010124     C     CEDAAS        ORNE      TASAAS
098900010124     C     CEDLNP        ORNE      TASLNP
099000010124     C     CEDNRS        ORNE      TASNRS
099100010124     C     CEDNSP        ORNE      TASNSP
099200010124     C     CEDTBL        ORNE      TASTBL
099300950307     C                   MOVEL     '1'           FINCED
099400001019     C                   ENDIF
099500950307     C*
099600950307     C                   ENDSR
099700950307     C*------------------------------------------------------------------------*
099800990802     C*? CHKTAS - CONTROLLA VALIDITA' DEL RECORD DI TITAS
099900950307     C*------------------------------------------------------------------------*
100000950309     C     CHKTAS        BEGSR
100100950307     C*
100200950307     C                   MOVEL     'S'           WRECOK            1
100300000329     C*
100400001023     C* CONTROLLA LO STATO DELLA CONSEGNA IN RELAZIONE AL TIPO BOLLA
100500001023     C* ==> SE TRATTASI DI BOLLA DI RECUPERO (F3/A3/AP) STORICIZZO COMUNQUE
100600001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
100700001018     C     TASDCM        IFEQ      *ZEROS                                       *NON CONSEGNATA
100800001023     C     TASTBL        ANDNE     'F3'
100900001023     C     TASTBL        ANDNE     'A3'
101000001023     C     TASTBL        ANDNE     'AP'
101100010222     C     TASCBO        ANDNE     'E '
101200010222     C     TASCBO        ANDNE     'B '
101300001019     C                   EXCEPT    ERRDCM
101400000329     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
101500001018     C                   ENDIF
101600001018     C                   ENDIF
101700950309     C*
101800950309     C* CONTROLLA IL CONTRASSEGNO
101900001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
102000950309     C     TASFCA        IFEQ      'S'                                          --- 1 -->
102100950309     C                   Z-ADD     TASAAS        CSBAAS
102200950309     C                   Z-ADD     TASLNP        CSBLNP
102300950309     C                   Z-ADD     TASNRS        CSBNRS
102400950309     C                   Z-ADD     TASNSP        CSBNSP
102500950309     C     KEYCSB        CHAIN     TNCSB03L                           99
102600950309     C     *IN99         IFEQ      '0'                                          --- 2 -->
102700950309     C     CSBSTA        IFNE      9                                            --- 3 -->
102800950309     C     CSBBNA        IFEQ      *ZEROS                                       *NON ANCORA PAGATO
102900950309     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
103000021021     C***                EXCEPT    NOCCSB                                       *STAMPO RECORD
103100021021     C                   ADD       1             CNTCSB                         *CONTEGGIO RECORD
103200950309     C                   ENDIF
103300950309     C                   ENDIF                                                  <-- 3 ---
103400950309     C                   ENDIF                                                  <-- 2 ---
103500950309     C                   ENDIF                                                  <-- 1 ---
103600001018     C                   ENDIF                                                  <-- 1 ---
103700000327     C*
103800000327     C* CONTROLLA I DANNI
103900001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
104000001018     C     TASFDA        IFEQ      'S'
104100040715      **
104200040715      **  Sostituito con il pgm FIDN12R che restituisce l'informazione
104300040715      **   sui danni fra bolla madre e figlia
104400040715      **
104500040715     C**                 Z-ADD     TASAAS        KCTAAS
104600040715     C**                 Z-ADD     TASLNP        KCTLNP
104700040715     C**                 Z-ADD     TASNRS        KCTNRS
104800040715     C**                 Z-ADD     TASNSP        KCTNSP
104900040715     C**   KEYDCT        CHAIN     FNDCT02L                           99
105000040715      *
105100040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
105200040715      * SIA COME MAMMA CHE COME FIGLIA
105300040715     c                   clear                   fidn12ds
105400040715     c                   eval      i12aas = TASAAS
105500040715     c                   eval      i12lnp = TASLNP
105600040715     c                   eval      i12nrs = TASNRS
105700040715     c                   eval      i12nsp = TASNSP
105800040715     c                   eval      i12fel = 'E'
105900040715     c                   call      'FIDN12R'
106000040715     c                   parm                    fidn12ds
106100040715      **
106200040715      * se c'è almeno una C.A. aperta
106300040715      **
106400040715     c     o12nca        ifgt      0
106500040715     C**   *IN99         IFEQ      *OFF                                         *TROVATO DANNO
106600040715      **
106700000327     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
106800021021     C***                EXCEPT    NOCDCT                                       *STAMPO RECORD
106900021021     C                   ADD       1             CNTDCT                         *CONTEGGIO RECORD
107000001018     C                   ENDIF
107100001018     C                   ENDIF
107200001018     C                   ENDIF
107300000404     C*
107400000404     C* CONTROLLA LE GIACENZE
107500001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
107600001018     C     TASFGC        IFEQ      'S'
107700000404     C                   Z-ADD     TASAAS        KCPAAS
107800000404     C                   Z-ADD     TASLNP        KCPLNP
107900000404     C                   Z-ADD     TASNRS        KCPNRS
108000000404     C                   Z-ADD     TASNSP        KCPNSP
108100000404     C                   Z-ADD     0             KCPFRG                         *PRIMO PRG
108200050301     C     KEYGCP        CHAIN(N)  TIGCP51L                           99
108300001018     C     *IN99         IFEQ      *OFF
108400001018     C     GCPFAS        IFLT      50                                           *FASI APERTE
108500000404     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
108600021021     C***                EXCEPT    NOCGCP                                       *STAMPO RECORD
108700021021     C                   ADD       1             CNTGCP                         *CONTEGGIO RECORD
108800001018     C                   ENDIF
108900001018     C                   ENDIF
109000001018     C                   ENDIF
109100001018     C                   ENDIF
109200011227     C*
109300020220     C* SE BOLLA POSTA CANCELLARE SOLO SE + VECCHIA D 2 ANNI
109400011227     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
109500011227     C     TASTSP        IFEQ      'P'                                          *BOLLA POSTA
109600020220     C     TASDSP        ANDGT     DSDATFP
109700011227     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
109800011227     C                   ENDIF
109900011227     C                   ENDIF
110000001019     C*
110100140109     C* DEVE Controllare: in caso di RESO controlla se la figlia è stata consegnata
110200140109     C*  altrimenti NON deve cancellare la MAMMA.
110300140109     C*
110400131216     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
110500131216     C                   IF        tasCCA = '2' or tasCCA = '6'                 *BOLLA RESO
110600131216     **
110700131223     C* Chiamata metodo GetLastChild
110800131223     c                   exsr      CHKCONS
110900140102     C                   IF         Consegnata_OK <> 'S'
111000140102     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
111100140102     C                   ENDIF
111200131223     **
111300131216     C                   ENDIF
111400131216     C                   ENDIF
111500131216     C*
111600131216     C*
111700131216     C*
111800001019     C* CONTEGGIO BOLLE SCARTATE
111900001019     C     WRECOK        IFEQ      'N'
112000001019     C                   ADD       1             NUMNON
112100001019     C                   ENDIF
112200950307     C*
112300950307     C                   ENDSR
112400131223     C*------------------------------------------------------------------------*
112500131223     C* CHKCONS  - Verifica se bolla effettivamente consegnata
112600131223     C*------------------------------------------------------------------------*
112700131223     C     CHKCONS       BEGSR
112800131223     C*
112900140102     C                   movel     'N'           Consegnata_OK     1
113000131223     C                   movel     *blanks       wEsito1           1
113100131223     C                   movel     *blanks       wEsito2           1
113200131223     C*
113300131223     C                   eval      wAAS = tasAAS
113400131223     C                   eval      wLNP = tasLNP
113500131223     C                   eval      wNRS = tasNRS
113600131223     C                   eval      wNSP = tasNSP
113700131223     C*
113800131223     C* Chiamata metodo GetLblTyp
113900131223     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
114000131223     C                                                wAAS
114100131223     C                                               :wLNP
114200131223     C                                               :wNRS
114300131223     C                                               :wNSP
114400131223     C                                               :pOutLblTyp
114500131223     C                                               :pOutAnnoBO
114600131223     C                                               :pOutLineaParBO
114700131223     C                                               :pOutSerieBO
114800131223     C                                               :pOutNumSpedBO
114900131223     C                                               :pOutDcmBO
115000131223     C                                               :pOutCcaBO
115100131223     C                                               :pOutRblBO))
115200131223     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
115300131223     C                   if        pOutLblTyp <> 'O'
115400131223     C                   eval      wAAS = pOutAnnoBO
115500131223     C                   eval      wLNP = pOutLineaParBO
115600131223     C                   eval      wNRS = pOutSerieBO
115700131223     C                   eval      wNSP = pOutNumSpedBO
115800131223     C                   endif
115900131223     C*
116000131223     C* Chiamata metodo GetLastChild
116100131223     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
116200131223     C                                                wAAS
116300131223     C                                               :wLNP
116400131223     C                                               :wNRS
116500131223     C                                               :wNSP
116600131223     C                                               :pOutAnnoFI
116700131223     C                                               :pOutLineaParFI
116800131223     C                                               :pOutSerieFI
116900131223     C                                               :pOutNumSpedFI
117000131223     C                                               :pOutDcmFI
117100131223     C                                               :pOutCcaFI))
117200131223     C*
117300140102     C* Considerazioni sullo stato di Consegna della bolla
117400140102     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)
117500140102     C                               or
117600131223     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
117700140102     C                   movel     'S'           Consegnata_OK     1
117800131223     C                   endif
117900131223     C*
118000131223     C                   ENDSR
118100950309     C*------------------------------------------------------------------------*
118200950309     C*?AGGTAB - AGGIORNA LA TABELLA '$3' CON ULTIMA DATA TRASF. TAS00 > TAS10
118300950309     C*------------------------------------------------------------------------*
118400950309     C     AGGTAB        BEGSR
118500950309     C*
118600950309     C                   OPEN      TABEL00F
118700950309     C*
118800950309     C                   Z-ADD     1             KTBUTE
118900950309     C                   MOVEL     '$3'          KTBCOD
119000950309     C                   MOVEL     '1'           KTBKEY
119100950309     C     KEYTAB        CHAIN     TABEL00F                           99
119200950309     C     *IN99         IFEQ      *ON                                          *NON TROVATA
119300950309     C                   ELSE                                                   *TROVATA
119400950309     C                   MOVEL     TBLUNI        DS$3
119500950309     C                   Z-ADD     PARDTF        §$3DP0                         *ULT.DATA TRASF P>SA
119600020220     C                   Z-ADD     PARDFP        §$3DPP                         *ULT.DATA TRASF P>SA
119700950309     C                   MOVEL     DS$3          TBLUNI
119800950309     C                   UPDATE    TABEL
119900950309     C                   ENDIF
120000950309     C*
120100950309     C                   CLOSE     TABEL00F
120200950309     C*
120300950309     C                   ENDSR
120400950717     C*------------------------------------------------------------------------*
120500950717     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
120600950717     C*------------------------------------------------------------------------*
120700950717     C     SUBDAT        BEGSR
120800950717     C                   TIME                    DTTIME
120900950717     C                   Z-ADD     DTANN         DSANN
121000950717     C                   Z-ADD     DTMES         DSMES
121100950717     C                   Z-ADD     DTGIO         DSGIO
121200950717     C                   Z-ADD     DTSEC         DSSEC
121300950717     C                   ENDSR
121400940912     C*------------------------------------------------------------------------*
121500050705     C*? funzione per scivere file x Laguna
121600940912     C*------------------------------------------------------------------------*
121700050705     C     srlaguna      BEGSR
121800050705     c                   clear                   tnsb34ds
121900050929     c* ricavo il progressivo da impostare nella directory ogni 450000 bolle
122000050720     c* cancellate cambio directory
122100120709      * 09/07/2012 con Bocchi verificato che il progressivo non serve più
122200120709     C**!!!              ADD       1             NUMlag                         *REC TRASF TITAS
122300120709     c**!!!numlag        div       450000        sb34prg
122400120709     c**!!!              add       1             sb34prg
122500050705     c                   eval      sb34tla = 'R'
122600050705     c                   eval      sb34aas = tasaas
122700050705     c                   eval      sb34lnp = taslnp
122800050705     c                   eval      sb34nrs = tasnrs
122900050705     c                   eval      sb34nsp = tasnsp
123000150123      *
123100150123      ** andando a pulire di 2 mesi in 2 mesi
123200150123      ** occorre introdurre un nuovo ragionamento sul nome della DIRECTORY di LAGUNA
123300150123      ** determinata nel TNSB34R sulla base della DATA passata in "SB34DTP"
123400050706     c                   eval      sb34dtp = pardtf
123500150123      *
123600150123      ** Prima andando mese x mese NON esisteva il problema:
123700150123      *  Se la spedizione non appartiene all'Anno/Mese della data "FINO AL" (PARDTF)
123800150123      *   del range di pulizia passato (pardti - PARDTF = dal-al), si deve
123900150123      **  sottrarre un mese alla data SB34DTP in modo che LAGUNA imposti il nome
124000150123      **  della Directory sul primo mese dei 2 che si sta pulendo.
124100150123     c                   if        TASanme <> PARDTFanme
124200150123     c                   eval      sb34dtp = pardtf_1m
124300150123     c                   end
124400150123      *
124500050706     c                   eval      sb34cop = parcop
124600050705     c                   call      'TNSB34R'
124700050705     c                   parm                    tnsb34ds
124800050705     c                   if        sb34err <> ' '
124900050705     c                   end
125000050705     C                   ENDSR
125100120709     C*------------------------------------------------------------------------*
125200120709     C*? funzione per scivere file x JDOC
125300120709     C*------------------------------------------------------------------------*
125400120709     C     srJdoc        BEGSR
125500120709     c                   clear                   tnsb34ds
125600120709     c                   eval      sb34tla = 'R'
125700120709     c                   eval      sb34aas = tasaas
125800120709     c                   eval      sb34lnp = taslnp
125900120709     c                   eval      sb34nrs = tasnrs
126000120709     c                   eval      sb34nsp = tasnsp
126100120709     c                   eval      sb34dtp = pardtf
126200120709     c                   eval      sb34cop = parcop
126300120709     c                   call      'TNSB34R1'
126400120709     c                   parm                    tnsb34ds
126500120709     c                   if        sb34err <> ' '
126600120709     c                   end
126700120709     C                   ENDSR
126800061122     C*------------------------------------------------------------------------*
126900061122     C*? WRTTASSTO - SCRITTURA FILE STORICO TITASS
127000061122     C*------------------------------------------------------------------------*
127100061122     C     WRTTASSTO     BEGSR
127200080312     c* memorizzo directory laguna per sapere dove è stata archiviata la spe
127300080312     c                   clear                   tasdtfp
127400080312     c                   if        §floiml <> ' '
127500080312     c                   move      tasaas        ladaas
127600080312     c                   move      taslnp        ladlnp
127700080312     c                   move      tasnrs        ladnrs
127800080312     c                   move      tasnsp        ladnsp
127900080312     c     klad          chain     tilad01l
128000080312     c                   If        %found(tilad01l)
128100080312     c                   movel     laddir        tasdtfp
128200080312     c                   end
128300080312     c                   end
128400061122      * Decodifica padroncino
128500061122     c                   If        taspdc <> *zeros
128600061122     c     Keyapd        chain     fiapd01l
128700061122     c                   If        %found(fiapd01l)
128800061122     c                   eval      taspdd = apdrsc
128900061122     c                   endif
129000061122     c                   endif
129100061122
129200061122      * recupero c/assegno
129300061122     c                   if        tasfca = 'S'
129400061122     c                   z-add     csbcas        Tascas
129500061122     c                   movel     csbvca        tasvca
129600061122     c                   endif
129700061122
129800061122     c                   move      tasksc        com04
129900061122      * PARTITA IVA / CODICE FISCALE
130000061122     c                   If        %subst(tastbl:1:1) = 'A'  and
130100061122     c                             tascpd <> *blanks
130200061122     c                   eval      tasiva  = tascpd
130300061122     c                   endif
130400061122      * se partita iva non valorizzata
130500061122     c                   If        tasiva = *blanks  or tascdf = *blanks
130600061122      * codificato franco o assegnato
130700061122     c                   if        com04  <>  9999  and com04  <>  8888
130800061122     c                   exsr      reciva
130900061122     c                   if        o69err <> *on
131000061122     c                   if        tascdf = *blanks
131100061122     c                   eval      tasiva = indiva
131200061122     c                   endif
131300061122      * se devo cercare anche il codife fiscale
131400061122     c                   if        tascdf = *blanks
131500061122     c                   eval      tascdf = indcdf
131600061122     c                   endif
131700061122
131800061122     c                   endif
131900061122     c                   endif
132000061122     c                   endif
132100061124      * valorizzo il flag che mi identifica le spedizioni senza segnacollo
132200091229     c                   If        Tasfns <> 'S' and tasncd = *zeros
132300061124     c                   eval      tassss = 'S'
132400061124     c                   endif
132500061122
132600061122     c                   write     titassto                             99
132700061122     C   99              EXCEPT    ERRsto
132800061122     C   99              MOVEL     '1'           FINL00                         *FINE PGM
132900061122     C  N99              ADD       1             NUMsto                         *REC TRASF FNEVB
133000061122
133100061122     C                   ENDSR
133200061122     C*
133300050705     C*------------------------------------------------------------------------*
133400050705     C*? RUTINZ - ROUTINE INIZIALE
133500050705     C*------------------------------------------------------------------------*
133600050705     C     RUTINZ        BEGSR
133700931115     C*
133800931115     C* PARAMETRI IN INPUT AL PGM
133900931115     C     *ENTRY        PLIST
134000931115     C                   PARM                    KPJBA
134100931115     C                   MOVEL     KPJBU         PARAM
134200950309     C*
134300150123     C*  Avendo cambiato il modo di pulire da mese x mese in due mesi in due mesi
134400150123     C*   devo preparare una data che verrà utilizzata da Laguna che corrisponde
134500150123     C*   alla data finale del range passato meno 1 mese -->  quindi la data di
134600150123     C*   fine mese del mese precedente a quella del range (PARDTF-1m)
134700150128     c                   move      parDTF        Data08            8 0
134800150128     c                   move      '01'          Data08
134900150128     c                   move      Data08        Data_ISO
135000150128      * prende il fine mese precedente e se lo memorizza
135100150128     c                   subdur    1:*DAYS       Data_ISO
135200150123     c                   move      Data_ISO      parDTF_1m
135300150123     C*
135400150128     C*
135500950309     C* DEFINISCO CAMPI
135600950309     C     *LIKE         DEFINE    TBLKUT        KTBUTE
135700950309     C     *LIKE         DEFINE    TBLCOD        KTBCOD
135800950309     C     *LIKE         DEFINE    TBLKEY        KTBKEY
135900970903     C     *LIKE         DEFINE    CETNBC        MINNBC
136000040715     C***  *LIKE         DEFINE    DCTAAS        KCTAAS                         *FNDCT00F
136100040715     C***  *LIKE         DEFINE    DCTLNP        KCTLNP
136200040715     C***  *LIKE         DEFINE    DCTNRS        KCTNRS
136300040715     C***  *LIKE         DEFINE    DCTNSP        KCTNSP
136400050301     C     *LIKE         DEFINE    GCPAAS        KCPAAS                         *TIGCP00F
136500000404     C     *LIKE         DEFINE    GCPLNP        KCPLNP
136600000404     C     *LIKE         DEFINE    GCPNRS        KCPNRS
136700000404     C     *LIKE         DEFINE    GCPNSP        KCPNSP
136800000404     C     *LIKE         DEFINE    GCPFRG        KCPFRG
136900050301     C     *LIKE         DEFINE    GAPAAS        KAPAAS                         *TIGAP00F
137000000404     C     *LIKE         DEFINE    GAPLNP        KAPLNP
137100000404     C     *LIKE         DEFINE    GAPNRS        KAPNRS
137200000404     C     *LIKE         DEFINE    GAPNSP        KAPNSP
137300050301     C     *LIKE         DEFINE    GNPAGC        KNPAGC                         *TIGNP00F
137400000404     C     *LIKE         DEFINE    GNPFGC        KNPFGC
137500000404     C     *LIKE         DEFINE    GNPNGC        KNPNGC
137600950309     C*
137700950309     C* CHIAVE LETTURA TABEL00F - COMPLETA
137800950309     C     KEYTAB        KLIST
137900950309     C                   KFLD                    KTBUTE
138000950309     C                   KFLD                    KTBCOD
138100950309     C                   KFLD                    KTBKEY
138200080925     C     KEYTAB1       KLIST
138300080925     C                   KFLD                    KTBUTE
138400080925     C                   KFLD                    KTBCOD
138500080925     C                   Z-ADD     1             KTBUTE
138600080925     C                   MOVEL     'TB'          KTBCOD
138700950309     C*
138800950309     C* CHIAVE LETTURA TNCSB01L - COMPLETA
138900950309     C     KEYCSB        KLIST
139000950309     C                   KFLD                    CSBAAS
139100950309     C                   KFLD                    CSBLNP
139200950309     C                   KFLD                    CSBNRS
139300950309     C                   KFLD                    CSBNSP
139400000327     C*
139500000327     C* CHIAVE LETTURA FNDCT02L - COMPLETA
139600040715     C***  KEYDCT        KLIST
139700040715     C***                KFLD                    KCTAAS
139800040715     C***                KFLD                    KCTLNP
139900040715     C***                KFLD                    KCTNRS
140000040715     C***                KFLD                    KCTNSP
140100930208     C*
140200990802     C* CHIAVE LETTURA TITAS38C - PARZIALE
140300930621     C     KEYTAS        KLIST
140400941123     C                   KFLD                    TASAAS
140500941123     C                   KFLD                    TASMGS
140600950307     C*
140700990802     C* CHIAVE LETTURA TITA730C - COMPLETA
140800950307     C     KEYTA7        KLIST
140900950307     C                   KFLD                    TA7AAS
141000950307     C                   KFLD                    TA7LNP
141100950307     C                   KFLD                    TA7NRS
141200950307     C                   KFLD                    TA7NSP
141300950307     C                   KFLD                    TA7TBL
141400990802     C*
141500990802     C* CHIAVE LETTURA TITAA30C - COMPLETA
141600990802     C     KEYTAA        KLIST
141700990802     C                   KFLD                    TAAAAS
141800990802     C                   KFLD                    TAALNP
141900990802     C                   KFLD                    TAANRS
142000990802     C                   KFLD                    TAANSP
142100990802     C*
142200990802     C* CHIAVE LETTURA TITAT30C - COMPLETA
142300990802     C     KEYTAT        KLIST
142400990802     C                   KFLD                    TATAAS
142500990802     C                   KFLD                    TATLNP
142600990802     C                   KFLD                    TATNRS
142700990802     C                   KFLD                    TATNSP
142800990802     C*
142900990802     C* CHIAVE LETTURA TITA430C - COMPLETA
143000990802     C     KEYTA4        KLIST
143100990802     C                   KFLD                    TA4AAS
143200990802     C                   KFLD                    TA4LNP
143300990802     C                   KFLD                    TA4NRS
143400990802     C                   KFLD                    TA4NSP
143500010903     C*
143600010903     C* CHIAVE LETTURA TITAH30C - COMPLETA
143700010903     C     KEYTAH        KLIST
143800010903     C                   KFLD                    TAHAAS
143900010903     C                   KFLD                    TAHLNP
144000010903     C                   KFLD                    TAHNRS
144100010903     C                   KFLD                    TAHNSP
144200950209     C*
144300950307     C* CHIAVE LETTURA ECCET30C - COMPLETA
144400950220     C     KEYCET        KLIST
144500950220     C                   KFLD                    CETAAS                         ANNO SPEDIZIONE
144600950220     C                   KFLD                    CETLNP                         LINEA DI PARTENZA
144700950220     C                   KFLD                    CETNRS                         NUMERO SERIE
144800950220     C                   KFLD                    CETNSP                         NUMERO SPEDIZIONE
144900950220     C                   KFLD                    CETTBL                         TIPO BOLLA
145000950307     C*
145100950307     C* CHIAVE LETTURA ECCED30C - COMPLETA
145200950307     C     KEYCED        KLIST
145300950307     C                   KFLD                    CEDAAS                         ANNO SPEDIZIONE
145400950307     C                   KFLD                    CEDLNP                         LINEA DI PARTENZA
145500950307     C                   KFLD                    CEDNRS                         NUMERO SERIE
145600950307     C                   KFLD                    CEDNSP                         NUMERO SPEDIZIONE
145700950307     C                   KFLD                    CEDTBL                         TIPO BOLLA
145800980325     C*
145900980325     C* CHIAVE LETTURA ECFIM30C - COMPLETA
146000980325     C     KEYFIM        KLIST
146100980325     C                   KFLD                    FIMAAS                         ANNO SPEDIZIONE
146200980325     C                   KFLD                    FIMLNP                         LINEA DI PARTENZA
146300980325     C                   KFLD                    FIMNRS                         NUMERO SERIE
146400980325     C                   KFLD                    FIMNSP                         NUMERO SPEDIZIONE
146500980325     C                   KFLD                    FIMTBL                         TIPO BOLLA
146600000327     C*
146700000327     C* CHIAVE LETTURA FNEVB01L - COMPLETA
146800000327     C     KEYEVB        KLIST
146900000327     C                   KFLD                    EVBAAS                         ANNO SPEDIZIONE
147000000327     C                   KFLD                    EVBLNP                         LINEA DI PARTENZA
147100000327     C                   KFLD                    EVBNRS                         NUMERO SERIE
147200000327     C                   KFLD                    EVBNSP                         NUMERO SPEDIZIONE
147300000404     C*
147400050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
147500000404     C     KEYGCP        KLIST
147600000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
147700000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
147800000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
147900000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
148000000404     C                   KFLD                    KCPFRG                         PRG RIAPERTURE
148100000404     C*
148200050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
148300000404     C     KE2GCP        KLIST
148400000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
148500000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
148600000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
148700000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
148800000404     C*
148900050301     C* CHIAVE LETTURA TIGAP01L - PARZIALE
149000000404     C     KEYGAP        KLIST
149100000404     C                   KFLD                    KAPAAS                         ANNO SPEDIZIONE
149200000404     C                   KFLD                    KAPLNP                         LINEA DI PARTENZA
149300000404     C                   KFLD                    KAPNRS                         NUMERO SERIE
149400000404     C                   KFLD                    KAPNSP                         NUMERO SPEDIZIONE
149500000404     C*
149600050301     C* CHIAVE LETTURA TIGNP01L - PARZIALE
149700000404     C     KEYGNP        KLIST
149800000404     C                   KFLD                    KNPAGC                         ANNO APERTURA
149900000404     C                   KFLD                    KNPFGC                         FILILAE APERTURA
150000000404     C                   KFLD                    KNPNGC                         NUMERO GIACENZA
150100040303     C*
150200040531     C* CHIAVE LETTURA FIAR531C
150300040303     C     KEYAR5        KLIST
150400040303     C                   KFLD                    AR5AAS
150500040303     C                   KFLD                    AR5LNP
150600040303     C                   KFLD                    AR5NRS
150700040303     C                   KFLD                    AR5NSP
150800120709
150900120709     C     KEYAR51       KLIST
151000120709     C                   KFLD                    AR5AAS
151100120709     C                   KFLD                    AR5LNP
151200120709     C                   KFLD                    AR5NRS
151300120709     C                   KFLD                    AR5NSP
151400120709     C                   KFLD                    AR5TRD
151500060919     C*
151600060919     C* CHIAVE LETTURA TITAI01L
151700060919     C     KEYtai        KLIST
151800060919     C                   KFLD                    taiAAS
151900060919     C                   KFLD                    taiLNP
152000060919     C                   KFLD                    taiNRS
152100060919     C                   KFLD                    taiNSP
152200060919     C                   KFLD                    taiTBL
152300081103
152400081103     C* CHIAVE LETTURA TITAI01L
152500081103     C     KEYrab        KLIST
152600081103     C                   KFLD                    rabAAS
152700081103     C                   KFLD                    rabLNP
152800081103     C                   KFLD                    rabNRS
152900081103     C                   KFLD                    rabNSP
153000061122
153100061122     C* CHIAVE LETTURA Fiapd01l
153200061122     C     KEYapd        KLIST
153300061122     C                   KFLD                    trcapd
153400061122     C                   KFLD                    taspdc
153500040823      *
153600040823     C     kbol          klist
153700040823     C                   kfld                    tasaas
153800040823     C                   kfld                    taslnp
153900040823     C                   kfld                    tasnrs
154000040823     C                   kfld                    tasnsp
154100080312      *
154200080312     C     klad          klist
154300080312     C                   kfld                    LADAAS
154400080312     C                   kfld                    LADLNP
154500080312     C                   kfld                    LADNRS
154600080312     C                   kfld                    LADNSP
154700000327     C*
154800131029     C     Kevb2f        KLIST
154900131029     C                   KFLD                    EVBAAS
155000131029     C                   KFLD                    EVBLNP
155100131029     C                   KFLD                    EVBNRS
155200131029     C                   KFLD                    EVBNSP
155300131029     C                   KFLD                    EVBDTV
155400131029     C                   KFLD                    EVBORV
155500131029     C                   KFLD                    EVBCEV
155600000327     C* CALCOLA LA DATA CORRENTE
155700000327     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
155800000327     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
155900000327     C                   Z-ADD     N8            G08DAT
156000000327     C                   Z-ADD     *ZEROS        G08INV
156100000327     C                   MOVEL     '0'           G08ERR
156200000327     C                   CALL      'XSRDA8'
156300000327     C                   PARM                    WLBDA8
156400000327     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
156500930208     C*
156600930208     C* IMPOSTO A ZERO IL FINE PGM
156700930208     C                   MOVEL     '0'           FINL00            1
156800931105     C                   MOVEL     *BLANKS       RSUT             20
156900040922     C                   Z-ADD     *ZEROS        NUMblp            9 0
157000040922     C                   Z-ADD     *ZEROS        NUMarb            9 0
157100950308     C                   Z-ADD     *ZEROS        NUMTAS            9 0
157200120709     C**!!!              Z-ADD     *ZEROS        NUMlag            9 0
157300950308     C                   Z-ADD     *ZEROS        NUMTA7            9 0
157400990802     C                   Z-ADD     *ZEROS        NUMTAA            9 0
157500990802     C                   Z-ADD     *ZEROS        NUMTAT            9 0
157600990802     C                   Z-ADD     *ZEROS        NUMTA4            9 0
157700010903     C                   Z-ADD     *ZEROS        NUMTAH            9 0
157800950307     C                   Z-ADD     *ZEROS        NUMCET            9 0
157900950308     C                   Z-ADD     *ZEROS        NUMCED            9 0
158000980325     C                   Z-ADD     *ZEROS        NUMFIM            9 0
158100970904     C                   Z-ADD     *ZEROS        NUMECT            9 0
158200970904     C                   Z-ADD     *ZEROS        NUMECD            9 0
158300000327     C                   Z-ADD     *ZEROS        NUMEVB            9 0
158400000404     C                   Z-ADD     *ZEROS        NUMGCP            9 0
158500000404     C                   Z-ADD     *ZEROS        NUMGAP            9 0
158600000404     C                   Z-ADD     *ZEROS        NUMGNP            9 0
158700001019     C                   Z-ADD     *ZEROS        NUMNON            9 0
158800040303     C                   Z-ADD     *ZEROS        NUMAR5            9 0
158900060919     C                   Z-ADD     *ZEROS        NUMtai            9 0
159000081103     C                   Z-ADD     *ZEROS        NUMrab            9 0
159100061122     C                   Z-ADD     *ZEROS        NUMsto            9 0
159200021021     C                   Z-ADD     *ZEROS        CNTGCP            9 0
159300021021     C                   Z-ADD     *ZEROS        CNTDCT            9 0
159400021021     C                   Z-ADD     *ZEROS        CNTCSB            9 0
159500040823      * Apro il Fnblp di filiale
159600040823     c                   Clear                   comman
159700040823     c                   Movel(p)  cmd(1)        comman
159800040823     c                   Eval      %Subst(comman:13:8) = 'FNBLP01L'
159900040910      *test sistema informativo di prova
160000040910     c                   if        %subst(knsif:7:1) = 'P'
160100040910     c                   Eval      %Subst(comman:30:10) = wlibp
160200040910     c                   else
160300040910     c                   Eval      %Subst(comman:30:10) = wlib
160400040910     c                   endif
160500040910
160600040823     c                   Eval      comman =
160700040823     c                             %trim(comman) + '/FNBLP01L)'
160800040823     c                   Call      'QCMDEXC'                            99
160900040823     c                   Parm                    comman
161000040823     c                   Parm                    lenght
161100040823     c                   If        *In99
161200040823     c                   seton                                        lr
161300040823     c                   Else
161400040823     c                   Open      Fnblp01l
161500040823     c                   EndIf
161600040823      *
161700040823      * Apro  Fnarb di filiale
161800040823     c                   Clear                   comman
161900040823     c                   Movel(p)  cmd(1)        comman
162000040823     c                   Eval      %Subst(comman:13:8) = 'FNARB01L'
162100040910      *test sistema informativo di prova
162200040910     c                   if        %subst(knsif:7:1) = 'P'
162300040910     c                   Eval      %Subst(comman:30:10) = wlibp
162400040910     c                   else
162500040910     c                   Eval      %Subst(comman:30:10) = wlib
162600040910     c                   endif
162700040910
162800040823     c                   Eval      comman =
162900040823     c                             %trim(comman) + '/FNARB01L)'
163000040823     c                   Call      'QCMDEXC'                            99
163100040823     c                   Parm                    comman
163200040823     c                   Parm                    lenght
163300040823     c                   If        *In99
163400040823     c                   seton                                        lr
163500040823     c                   Else
163600040823     c                   Open      Fnarb01l
163700040823     c                   EndIf
163800040823      *
163900931115     C*
164000950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
164100080925     c* carico i tipi bolla RECUPERO per evitare di salvarli nel TITASS0F
164200081103     c                   clear                   conta
164300080926     c                   open      tabel00f
164400080925     c     keytab1       setll     tabel00f
164500080925     c                   do        *hival
164600080925     c     keytab1       reade(n)  tabel00f
164700080925     c                   if        %eof(tabel00f)
164800080925     c                   leave
164900080925     c                   end
165000080925     c                   movel(p)  tbluni        dstb
165100080925     c                   if        §TBRBL ='R'
165200080925     c                   add       1             conta             2 0
165300080925     c                   movel     tblkey        tbl(conta)
165400080925     c                   end
165500080925     c                   enddo
165600080926     c                   close     tabel00f
165700000000     C*
165800000000     C                   ENDSR
165900940912     O*------------------------------------------------------------------------*
166000940905     OPRTF198   E            TESTA            02
166100950309     O                                            8 'TNSB38R '
166200931105     O                       RSUT                45
166300990802     O                                           82 'TRASF. TITASP -> SALVAT.'
166400931115     O                                           90 'DAL:'
166500950309     O                       PARDTI              99
166600931115     O                                          110 'AL:'
166700950309     O                       PARDTF             119
166800020220     O                                          128 '/PT'
166900020220     O                       PARDFP             132
167000931105     O                                          150 'Data:'
167100950717     O                       DSDAT6             159 '  /  /  '
167200931105     O                                          167 'Ora:'
167300950717     O                       DTORA6             178 '  .  .  '
167400931105     O                                          190 'Pagina:'
167500931105     O                       PAGE          Z    198
167600931105     O          E            TESTA       1  1
167700931105     O                       TES(2)              66
167800931105     O                       TES(3)             132
167900931105     O                       TES(4)             198
168000931105     O          E            TESTA          1
168100931105     O                       TES(1)              66
168200931105     O                       TES(1)             132
168300931105     O                       TES(1)             198
168400001019     O*------------------------------------
168500001019     O          E            ERRDCM         1
168600001019     O                       TASAAS            +  5
168700001019     O                       TASLNP            +  1
168800001019     O                       TASNRS            +  1
168900001019     O                       TASNSP            +  1
169000001019     O                       TASTBL            +  1
169100001019     O                                         +  1 'BOLLA NON CONS.    '
169200931105     O*------------------------------------
169300950307     O          E            ERRTAS         1
169400950307     O                       TASAAS            +  5
169500950307     O                       TASLNP            +  1
169600950307     O                       TASNRS            +  1
169700950307     O                       TASNSP            +  1
169800950307     O                       TASTBL            +  1
169900990802     O                                         +  1 'ERR. WRITE TITAS   '
170000950307     O*------------------------------------
170100950307     O          E            ERRTA7         1
170200950307     O                       TA7AAS            +  5
170300950307     O                       TA7LNP            +  1
170400950307     O                       TA7NRS            +  1
170500950307     O                       TA7NSP            +  1
170600950307     O                       TA7TBL            +  1
170700950307     O                       TA7SVN            +  1
170800990802     O                                         +  1 'ERR. WRITE TITA7   '
170900990802     O*------------------------------------
171000990802     O          E            ERRTAA         1
171100990802     O                       TAAAAS            +  5
171200990802     O                       TAALNP            +  1
171300990802     O                       TAANRS            +  1
171400990802     O                       TAANSP            +  1
171500990802     O                       TAATRC            +  1
171600990802     O                                         +  1 'ERR. WRITE TITAA   '
171700990802     O*------------------------------------
171800990802     O          E            ERRTAT         1
171900990802     O                       TATAAS            +  5
172000990802     O                       TATLNP            +  1
172100990802     O                       TATNRS            +  1
172200990802     O                       TATNSP            +  1
172300990802     O                       TATNSC            +  1
172400990802     O                                         +  1 'ERR. WRITE TITAT   '
172500990802     O*------------------------------------
172600990802     O          E            ERRTA4         1
172700990802     O                       TA4AAS            +  5
172800990802     O                       TA4LNP            +  1
172900990802     O                       TA4NRS            +  1
173000990802     O                       TA4NSP            +  1
173100990802     O                       TA4TRC            +  1
173200990802     O                                         +  1 'ERR. WRITE TITA4   '
173300010903     O*------------------------------------
173400010903     O          E            ERRTAH         1
173500010903     O                       TAHAAS            +  5
173600010903     O                       TAHLNP            +  1
173700010903     O                       TAHNRS            +  1
173800010903     O                       TAHNSP            +  1
173900010903     O                       TAHTRC            +  1
174000010903     O                                         +  1 'ERR. WRITE TITAH   '
174100950307     O*------------------------------------
174200950307     O          E            ERRCET         1
174300950307     O                       CETAAS            +  5
174400950307     O                       CETLNP            +  1
174500950307     O                       CETNRS            +  1
174600950307     O                       CETNSP            +  1
174700950307     O                       CETTBL            +  1
174800950309     O                                         +  1 'ERR. WRITE ECCET   '
174900950307     O*------------------------------------
175000950307     O          E            ERRCED         1
175100950307     O                       CEDAAS            +  5
175200950307     O                       CEDLNP            +  1
175300950307     O                       CEDNRS            +  1
175400950307     O                       CEDNSP            +  1
175500950307     O                       CEDTBL            +  1
175600950307     O                       CEDCMP            +  1
175700950309     O                                         +  1 'ERR. WRITE ECCED   '
175800980325     O*------------------------------------
175900980325     O          E            ERRFIM         1
176000980325     O                       FIMAAS            +  5
176100980325     O                       FIMLNP            +  1
176200980325     O                       FIMNRS            +  1
176300980325     O                       FIMNSP            +  1
176400980325     O                       FIMTBL            +  1
176500980325     O                                         +  1 'ERR. WRITE ECFIM   '
176600970903     O*------------------------------------
176700970903     O          E            ERRECT         1
176800970903     O                       ECTNBC        Z   +  5
176900970903     O                       ECTCMP        Z   +  1
177000970903     O                       ECTDDC            +  1 '    /  /  '
177100970903     O                                         +  1 'ERR. WRITE ECECT   '
177200970903     O*------------------------------------
177300970903     O          E            ERRECD         1
177400970903     O                       ECDNBC        Z   +  5
177500970903     O                       ECDCMP        Z   +  1
177600970903     O                       ECDFRA            +  1
177700970903     O                       ECDIOE            +  1
177800970903     O                       ECDKCC        Z   +  1
177900970903     O                       ECDKSC        Z   +  1
178000970903     O                       ECDRIC            +  1
178100970903     O                       ECDCCF        Z   +  1
178200970903     O                       ECDDDO            +  1 '    /  /  '
178300970903     O                                         +  1 'ERR. WRITE ECECD   '
178400000404     O*------------------------------------
178500000404     O          E            ERREVB         1
178600000404     O                       EVBAAS            +  5
178700000404     O                       EVBLNP            +  1
178800000404     O                       EVBNRS            +  1
178900000404     O                       EVBNSP            +  1
179000000404     O                                         +  1 'ERR. WRITE FNEVB   '
179100000404     O*------------------------------------
179200000404     O          E            ERRGCP         1
179300000404     O                       GCPAAS            +  5
179400000404     O                       GCPLNP            +  1
179500000404     O                       GCPNRS            +  1
179600000404     O                       GCPNSP            +  1
179700050301     O                                         +  1 'ERR. WRITE TIGCP   '
179800050303     O          E            ERRGCP1        1
179900050303     O                       GCPAAS            +  5
180000050303     O                       GCPLNP            +  1
180100050303     O                       GCPNRS            +  1
180200050303     O                       GCPNSP            +  1
180300050303     O                                         +  1 'GIACENZA ANCORA IN FILIALE'
180400000404     O*------------------------------------
180500000404     O          E            ERRGAP         1
180600000404     O                       GAPAAS            +  5
180700000404     O                       GAPLNP            +  1
180800000404     O                       GAPNRS            +  1
180900000404     O                       GAPNSP            +  1
181000050301     O                                         +  1 'ERR. WRITE TIGAP   '
181100000404     O*------------------------------------
181200000404     O          E            ERRGNP         1
181300000404     O                       GNPAGC            +  5
181400000404     O                       GNPFGC            +  4
181500000404     O                       GNPNGC            +  1
181600050301     O                                         +  1 'ERR. WRITE TIGNP   '
181700040303     O*------------------------------------
181800040303     O          E            ERRAR5         1
181900040303     O                       AR5AAS            +  5
182000040303     O                       AR5LNP            +  1
182100040303     O                       AR5NRS            +  1
182200040303     O                       AR5NSP            +  1
182300040303     O                                         +  1 'ERR. WRITE FIAR5   '
182400060919     O          E            ERRtai         1
182500060919     O                       taiAAS            +  5
182600060919     O                       taiLNP            +  1
182700060919     O                       taiNRS            +  1
182800060919     O                       taiNSP            +  1
182900060919     O                       taitbl            +  1
183000060919     O                                         +  1 'ERR. WRITE TITAI   '
183100081103     O          E            ERRrab         1
183200081103     O                       rabAAS            +  5
183300081103     O                       rabLNP            +  1
183400081103     O                       rabNRS            +  1
183500081103     O                       rabNSP            +  1
183600081103     O                                         +  1 'ERR. WRITE FIRAB   '
183700950309     O*------------------------------------
183800000404     O          E            NOCCSB         1
183900950309     O                       CSBAAS            +  5
184000950309     O                       CSBLNP            +  1
184100950309     O                       CSBNRS            +  1
184200950309     O                       CSBNSP            +  1
184300000404     O                                         +  1 'BOLLA NON CANC:TNCSB'
184400000404     O*------------------------------------
184500040715     O*****     E            NOCDCT         1
184600040715     O*****                  DCTAAS            +  5
184700040715     O*****                  DCTLNP            +  1
184800040715     O*****                  DCTNRS            +  1
184900040715     O*****                  DCTNSP            +  1
185000040715     O*****                                    +  1 'BOLLA NON CANC:FNDCT'
185100040715     O          E            NOCDCT         1
185200040715     O                       i12AAS            +  5
185300040715     O                       i12LNP            +  1
185400040715     O                       i12NRS            +  1
185500040715     O                       i12NSP            +  1
185600040715     O                                         +  1 'BOLLA NON CANC:FNDCT'
185700000404     O*------------------------------------
185800000404     O          E            NOCGCP         1
185900000404     O                       GCPAAS            +  5
186000000404     O                       GCPLNP            +  1
186100000404     O                       GCPNRS            +  1
186200000404     O                       GCPNSP            +  1
186300050301     O                                         +  1 'BOLLA NON CANC:TIGCP'
186400040922     O*------------------------------------
186500040922     O          E            ERRblp         1
186600040922     O                       blpAAS            +  5
186700040922     O                       blpLNP            +  1
186800040922     O                       blpNRS            +  1
186900040922     O                       blpNSP            +  1
187000040922     O                                         +  1 'ERR. Trovato reK su FNBLP'
187100040922     O*------------------------------------
187200040922     O          E            ERRarb         1
187300040922     O                       arbAAS            +  5
187400040922     O                       arbLNP            +  1
187500040922     O                       arbNRS            +  1
187600040922     O                       arbNSP            +  1
187700040922     O                                         +  1 'ERR. Trovato reK su FNARB'
187800061122     O          E            ERRSTO         1
187900061122     O                       TASAAS            +  5
188000061122     O                       TASLNP            +  1
188100061122     O                       TASNRS            +  1
188200061122     O                       TASNSP            +  1
188300061122     O                       TASTBL            +  1
188400061122     O                                         +  1 'ERR. WRITE STORICO '
188500040922     O*------------------------------------
188600950308     O          E            STAFIN         1
188700990802     O                                           30 'N.REC.TRASF. TITAS'
188800950308     O                       NUMTAS        2     44
188900950308     O          E            STAFIN         1
189000990802     O                                           30 'N.REC.TRASF. TITA7'
189100950308     O                       NUMTA7        2     44
189200990802     O          E            STAFIN         1
189300990802     O                                           30 'N.REC.TRASF. TITAA'
189400990802     O                       NUMTAA        2     44
189500990802     O          E            STAFIN         1
189600990802     O                                           30 'N.REC.TRASF. TITAT'
189700990802     O                       NUMTAT        2     44
189800990802     O          E            STAFIN         1
189900990802     O                                           30 'N.REC.TRASF. TITA4'
190000990802     O                       NUMTA4        2     44
190100010903     O          E            STAFIN         1
190200010903     O                                           30 'N.REC.TRASF. TITAH'
190300010903     O                       NUMTAH        2     44
190400950308     O          E            STAFIN         1
190500950308     O                                           30 'N.REC.TRASF. ECCET'
190600950308     O                       NUMCET        2     44
190700950308     O          E            STAFIN         1
190800950308     O                                           30 'N.REC.TRASF. ECCED'
190900950308     O                       NUMCED        2     44
191000980325     O          E            STAFIN         1
191100980325     O                                           30 'N.REC.TRASF. ECFIM'
191200980325     O                       NUMFIM        2     44
191300970903     O          E            STAFIN         1
191400970903     O                                           30 'N.REC.TRASF. ECECT'
191500970903     O                       NUMECT        2     44
191600970903     O          E            STAFIN         1
191700970903     O                                           30 'N.REC.TRASF. ECECD'
191800970903     O                       NUMECD        2     44
191900000327     O          E            STAFIN         1
192000000327     O                                           30 'N.REC.TRASF. FNEVB'
192100000327     O                       NUMEVB        2     44
192200000404     O          E            STAFIN         1
192300050301     O                                           30 'N.REC.TRASF. TIGCP'
192400000404     O                       NUMGCP        2     44
192500000404     O          E            STAFIN         1
192600050301     O                                           30 'N.REC.TRASF. TIGAP'
192700000404     O                       NUMGAP        2     44
192800000404     O          E            STAFIN         1
192900050301     O                                           30 'N.REC.TRASF. TIGNP'
193000000404     O                       NUMGNP        2     44
193100040310     O          E            STAFIN         1
193200040310     O                                           30 'N.REC. BOLLE FIAR5'
193300040310     O                       NUMAR5        2     44
193400060919     O          E            STAFIN         1
193500060919     O                                           30 'N.REC. BOLLE TITAI'
193600060919     O                       NUMTAI        2     44
193700081103     O          E            STAFIN         1
193800081103     O                                           30 'N.REC. BOLLE FIRAB'
193900081103     O                       NUMRAB        2     44
194000061122     O          E            STAFIN         1
194100061122     O                                           30 'N.BOLLE IN STORICO'
194200061122     O                       NUMSTO        2     44
194300001019     O          E            STAFIN         1
194400001020     O                                           30 'N.REC. BOLLE SCART'
194500001019     O                       NUMNON        2     44
194600021021     O          E            STAFIN         1
194700021021     O                                           30 'DI CUI:'
194800021021     O          E            STAFIN         1
194900021021     O                                           30 'X GIACENZA APERTA '
195000021021     O                       CNTGCP        2     44
195100021021     O          E            STAFIN         1
195200021021     O                                           30 'X DANNO PENDENTE  '
195300021021     O                       CNTDCT        2     44
195400021021     O          E            STAFIN         1
195500021021     O                                           30 'X C/A ATTIVO      '
195600021021     O                       CNTCSB        2     44
195700040922     O          E            STAFIN         1
195800040922     O                                           30 'N.REK.Trovati FNBLP'
195900040922     O                       NUMblp        2     44
196000040922     O          E            STAFIN         1
196100040922     O                                           30 'N.REK.Trovati FNARB'
196200040922     O                       NUMarb        2     44
196300060626     O          E            STAFIN         1
196400060626     O                                           +0 'Chiusuara pgm: FINTAS'
196500060626     O                       fintas              +1
196600060626     O                       tasdsp              +1
196700060626     O                       dsdatf              +1
196800060626     O               98                          +1 'SHTDN'
196900060626     O                                           +1 'FINL00'
197000060626     O                       finl00              +1
197100950308     O          E            STAFIN           60
197200931115     O                                           76 '*** FINE STAMPA ***'
197300931115     O                                          150 'Data:'
197400950717     O                       DSDAT6             159 '  /  /  '
197500990802     O                                          167 'Ora:'
197600950717     O                       DTORA6             178 '  :  :  '
197700940912     O*------------------------------------------------------------------------*
197800970903     O* AGGIORNAMENTO FILES
197900970903     O*------------------------------------------------------------------------*
198000970903     OECECT     E            AGGECT
198100970903     O                       ECTATB
198200970903     O                       ECTFTR
198300970903     OECECD     E            AGGECD
198400970903     O                       ECDATB
198500970903     O                       ECDFTR
198600970903     O*------------------------------------------------------------------------*
198700040823** cmd
198800040823OVRDBF FILE(        ) TOFILE(
198900931105**  TES - RIGHE IN STAMPA
199000931105------------------------------------------------------------------  1
199100931105 DATA    ORA       AA LNP SE N.SPED. TB     ERR  DESCRIZIONE ERROR  2
199200950308E                                                                   3
199300931105                                                                    4
199400931105                                                                    5
199500931105                                                                    6
199600931105                                                                    7
199700931105                                                                    8
199800931105                                                                    9
199900931105                                                                   10
200000931105                                                                   11
200100931105                                                                   12
200200931105                                                                   13
200300931105                                                                   14
200400931105                                                                   15
