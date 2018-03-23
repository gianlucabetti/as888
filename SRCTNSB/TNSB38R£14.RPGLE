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
123600150302      *  COME PER LAGUNA anche per archiviare i DOCUMENTI
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
126200150302      *
126300150302      ** Prima andando mese x mese NON esisteva il problema:
126400150302      *  Se la spedizione non appartiene all'Anno/Mese della data "FINO AL" (PARDTF)
126500150302      *   del range di pulizia passato (pardti - PARDTF = dal-al), si deve
126600150302      **  sottrarre un mese alla data SB34DTP come fatto x LAGUNA impostando il nome
126700150302      **  della Directory sul primo mese dei 2 che si sta pulendo.
126800150302     c                   if        TASanme <> PARDTFanme
126900150302     c                   eval      sb34dtp = pardtf_1m
127000150302     c                   end
127100150302      *
127200120709     c                   eval      sb34cop = parcop
127300120709     c                   call      'TNSB34R1'
127400120709     c                   parm                    tnsb34ds
127500120709     c                   if        sb34err <> ' '
127600120709     c                   end
127700120709     C                   ENDSR
127800061122     C*------------------------------------------------------------------------*
127900061122     C*? WRTTASSTO - SCRITTURA FILE STORICO TITASS
128000061122     C*------------------------------------------------------------------------*
128100061122     C     WRTTASSTO     BEGSR
128200080312     c* memorizzo directory laguna per sapere dove è stata archiviata la spe
128300080312     c                   clear                   tasdtfp
128400080312     c                   if        §floiml <> ' '
128500080312     c                   move      tasaas        ladaas
128600080312     c                   move      taslnp        ladlnp
128700080312     c                   move      tasnrs        ladnrs
128800080312     c                   move      tasnsp        ladnsp
128900080312     c     klad          chain     tilad01l
129000080312     c                   If        %found(tilad01l)
129100080312     c                   movel     laddir        tasdtfp
129200080312     c                   end
129300080312     c                   end
129400061122      * Decodifica padroncino
129500061122     c                   If        taspdc <> *zeros
129600061122     c     Keyapd        chain     fiapd01l
129700061122     c                   If        %found(fiapd01l)
129800061122     c                   eval      taspdd = apdrsc
129900061122     c                   endif
130000061122     c                   endif
130100061122
130200061122      * recupero c/assegno
130300061122     c                   if        tasfca = 'S'
130400061122     c                   z-add     csbcas        Tascas
130500061122     c                   movel     csbvca        tasvca
130600061122     c                   endif
130700061122
130800061122     c                   move      tasksc        com04
130900061122      * PARTITA IVA / CODICE FISCALE
131000061122     c                   If        %subst(tastbl:1:1) = 'A'  and
131100061122     c                             tascpd <> *blanks
131200061122     c                   eval      tasiva  = tascpd
131300061122     c                   endif
131400061122      * se partita iva non valorizzata
131500061122     c                   If        tasiva = *blanks  or tascdf = *blanks
131600061122      * codificato franco o assegnato
131700061122     c                   if        com04  <>  9999  and com04  <>  8888
131800061122     c                   exsr      reciva
131900061122     c                   if        o69err <> *on
132000061122     c                   if        tascdf = *blanks
132100061122     c                   eval      tasiva = indiva
132200061122     c                   endif
132300061122      * se devo cercare anche il codife fiscale
132400061122     c                   if        tascdf = *blanks
132500061122     c                   eval      tascdf = indcdf
132600061122     c                   endif
132700061122
132800061122     c                   endif
132900061122     c                   endif
133000061122     c                   endif
133100061124      * valorizzo il flag che mi identifica le spedizioni senza segnacollo
133200091229     c                   If        Tasfns <> 'S' and tasncd = *zeros
133300061124     c                   eval      tassss = 'S'
133400061124     c                   endif
133500061122
133600061122     c                   write     titassto                             99
133700061122     C   99              EXCEPT    ERRsto
133800061122     C   99              MOVEL     '1'           FINL00                         *FINE PGM
133900061122     C  N99              ADD       1             NUMsto                         *REC TRASF FNEVB
134000061122
134100061122     C                   ENDSR
134200061122     C*
134300050705     C*------------------------------------------------------------------------*
134400050705     C*? RUTINZ - ROUTINE INIZIALE
134500050705     C*------------------------------------------------------------------------*
134600050705     C     RUTINZ        BEGSR
134700931115     C*
134800931115     C* PARAMETRI IN INPUT AL PGM
134900931115     C     *ENTRY        PLIST
135000931115     C                   PARM                    KPJBA
135100931115     C                   MOVEL     KPJBU         PARAM
135200950309     C*
135300150123     C*  Avendo cambiato il modo di pulire da mese x mese in due mesi in due mesi
135400150123     C*   devo preparare una data che verrà utilizzata da Laguna che corrisponde
135500150123     C*   alla data finale del range passato meno 1 mese -->  quindi la data di
135600150123     C*   fine mese del mese precedente a quella del range (PARDTF-1m)
135700150128     c                   move      parDTF        Data08            8 0
135800150128     c                   move      '01'          Data08
135900150128     c                   move      Data08        Data_ISO
136000150128      * prende il fine mese precedente e se lo memorizza
136100150128     c                   subdur    1:*DAYS       Data_ISO
136200150123     c                   move      Data_ISO      parDTF_1m
136300150123     C*
136400150128     C*
136500950309     C* DEFINISCO CAMPI
136600950309     C     *LIKE         DEFINE    TBLKUT        KTBUTE
136700950309     C     *LIKE         DEFINE    TBLCOD        KTBCOD
136800950309     C     *LIKE         DEFINE    TBLKEY        KTBKEY
136900970903     C     *LIKE         DEFINE    CETNBC        MINNBC
137000040715     C***  *LIKE         DEFINE    DCTAAS        KCTAAS                         *FNDCT00F
137100040715     C***  *LIKE         DEFINE    DCTLNP        KCTLNP
137200040715     C***  *LIKE         DEFINE    DCTNRS        KCTNRS
137300040715     C***  *LIKE         DEFINE    DCTNSP        KCTNSP
137400050301     C     *LIKE         DEFINE    GCPAAS        KCPAAS                         *TIGCP00F
137500000404     C     *LIKE         DEFINE    GCPLNP        KCPLNP
137600000404     C     *LIKE         DEFINE    GCPNRS        KCPNRS
137700000404     C     *LIKE         DEFINE    GCPNSP        KCPNSP
137800000404     C     *LIKE         DEFINE    GCPFRG        KCPFRG
137900050301     C     *LIKE         DEFINE    GAPAAS        KAPAAS                         *TIGAP00F
138000000404     C     *LIKE         DEFINE    GAPLNP        KAPLNP
138100000404     C     *LIKE         DEFINE    GAPNRS        KAPNRS
138200000404     C     *LIKE         DEFINE    GAPNSP        KAPNSP
138300050301     C     *LIKE         DEFINE    GNPAGC        KNPAGC                         *TIGNP00F
138400000404     C     *LIKE         DEFINE    GNPFGC        KNPFGC
138500000404     C     *LIKE         DEFINE    GNPNGC        KNPNGC
138600950309     C*
138700950309     C* CHIAVE LETTURA TABEL00F - COMPLETA
138800950309     C     KEYTAB        KLIST
138900950309     C                   KFLD                    KTBUTE
139000950309     C                   KFLD                    KTBCOD
139100950309     C                   KFLD                    KTBKEY
139200080925     C     KEYTAB1       KLIST
139300080925     C                   KFLD                    KTBUTE
139400080925     C                   KFLD                    KTBCOD
139500080925     C                   Z-ADD     1             KTBUTE
139600080925     C                   MOVEL     'TB'          KTBCOD
139700950309     C*
139800950309     C* CHIAVE LETTURA TNCSB01L - COMPLETA
139900950309     C     KEYCSB        KLIST
140000950309     C                   KFLD                    CSBAAS
140100950309     C                   KFLD                    CSBLNP
140200950309     C                   KFLD                    CSBNRS
140300950309     C                   KFLD                    CSBNSP
140400000327     C*
140500000327     C* CHIAVE LETTURA FNDCT02L - COMPLETA
140600040715     C***  KEYDCT        KLIST
140700040715     C***                KFLD                    KCTAAS
140800040715     C***                KFLD                    KCTLNP
140900040715     C***                KFLD                    KCTNRS
141000040715     C***                KFLD                    KCTNSP
141100930208     C*
141200990802     C* CHIAVE LETTURA TITAS38C - PARZIALE
141300930621     C     KEYTAS        KLIST
141400941123     C                   KFLD                    TASAAS
141500941123     C                   KFLD                    TASMGS
141600950307     C*
141700990802     C* CHIAVE LETTURA TITA730C - COMPLETA
141800950307     C     KEYTA7        KLIST
141900950307     C                   KFLD                    TA7AAS
142000950307     C                   KFLD                    TA7LNP
142100950307     C                   KFLD                    TA7NRS
142200950307     C                   KFLD                    TA7NSP
142300950307     C                   KFLD                    TA7TBL
142400990802     C*
142500990802     C* CHIAVE LETTURA TITAA30C - COMPLETA
142600990802     C     KEYTAA        KLIST
142700990802     C                   KFLD                    TAAAAS
142800990802     C                   KFLD                    TAALNP
142900990802     C                   KFLD                    TAANRS
143000990802     C                   KFLD                    TAANSP
143100990802     C*
143200990802     C* CHIAVE LETTURA TITAT30C - COMPLETA
143300990802     C     KEYTAT        KLIST
143400990802     C                   KFLD                    TATAAS
143500990802     C                   KFLD                    TATLNP
143600990802     C                   KFLD                    TATNRS
143700990802     C                   KFLD                    TATNSP
143800990802     C*
143900990802     C* CHIAVE LETTURA TITA430C - COMPLETA
144000990802     C     KEYTA4        KLIST
144100990802     C                   KFLD                    TA4AAS
144200990802     C                   KFLD                    TA4LNP
144300990802     C                   KFLD                    TA4NRS
144400990802     C                   KFLD                    TA4NSP
144500010903     C*
144600010903     C* CHIAVE LETTURA TITAH30C - COMPLETA
144700010903     C     KEYTAH        KLIST
144800010903     C                   KFLD                    TAHAAS
144900010903     C                   KFLD                    TAHLNP
145000010903     C                   KFLD                    TAHNRS
145100010903     C                   KFLD                    TAHNSP
145200950209     C*
145300950307     C* CHIAVE LETTURA ECCET30C - COMPLETA
145400950220     C     KEYCET        KLIST
145500950220     C                   KFLD                    CETAAS                         ANNO SPEDIZIONE
145600950220     C                   KFLD                    CETLNP                         LINEA DI PARTENZA
145700950220     C                   KFLD                    CETNRS                         NUMERO SERIE
145800950220     C                   KFLD                    CETNSP                         NUMERO SPEDIZIONE
145900950220     C                   KFLD                    CETTBL                         TIPO BOLLA
146000950307     C*
146100950307     C* CHIAVE LETTURA ECCED30C - COMPLETA
146200950307     C     KEYCED        KLIST
146300950307     C                   KFLD                    CEDAAS                         ANNO SPEDIZIONE
146400950307     C                   KFLD                    CEDLNP                         LINEA DI PARTENZA
146500950307     C                   KFLD                    CEDNRS                         NUMERO SERIE
146600950307     C                   KFLD                    CEDNSP                         NUMERO SPEDIZIONE
146700950307     C                   KFLD                    CEDTBL                         TIPO BOLLA
146800980325     C*
146900980325     C* CHIAVE LETTURA ECFIM30C - COMPLETA
147000980325     C     KEYFIM        KLIST
147100980325     C                   KFLD                    FIMAAS                         ANNO SPEDIZIONE
147200980325     C                   KFLD                    FIMLNP                         LINEA DI PARTENZA
147300980325     C                   KFLD                    FIMNRS                         NUMERO SERIE
147400980325     C                   KFLD                    FIMNSP                         NUMERO SPEDIZIONE
147500980325     C                   KFLD                    FIMTBL                         TIPO BOLLA
147600000327     C*
147700000327     C* CHIAVE LETTURA FNEVB01L - COMPLETA
147800000327     C     KEYEVB        KLIST
147900000327     C                   KFLD                    EVBAAS                         ANNO SPEDIZIONE
148000000327     C                   KFLD                    EVBLNP                         LINEA DI PARTENZA
148100000327     C                   KFLD                    EVBNRS                         NUMERO SERIE
148200000327     C                   KFLD                    EVBNSP                         NUMERO SPEDIZIONE
148300000404     C*
148400050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
148500000404     C     KEYGCP        KLIST
148600000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
148700000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
148800000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
148900000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
149000000404     C                   KFLD                    KCPFRG                         PRG RIAPERTURE
149100000404     C*
149200050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
149300000404     C     KE2GCP        KLIST
149400000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
149500000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
149600000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
149700000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
149800000404     C*
149900050301     C* CHIAVE LETTURA TIGAP01L - PARZIALE
150000000404     C     KEYGAP        KLIST
150100000404     C                   KFLD                    KAPAAS                         ANNO SPEDIZIONE
150200000404     C                   KFLD                    KAPLNP                         LINEA DI PARTENZA
150300000404     C                   KFLD                    KAPNRS                         NUMERO SERIE
150400000404     C                   KFLD                    KAPNSP                         NUMERO SPEDIZIONE
150500000404     C*
150600050301     C* CHIAVE LETTURA TIGNP01L - PARZIALE
150700000404     C     KEYGNP        KLIST
150800000404     C                   KFLD                    KNPAGC                         ANNO APERTURA
150900000404     C                   KFLD                    KNPFGC                         FILILAE APERTURA
151000000404     C                   KFLD                    KNPNGC                         NUMERO GIACENZA
151100040303     C*
151200040531     C* CHIAVE LETTURA FIAR531C
151300040303     C     KEYAR5        KLIST
151400040303     C                   KFLD                    AR5AAS
151500040303     C                   KFLD                    AR5LNP
151600040303     C                   KFLD                    AR5NRS
151700040303     C                   KFLD                    AR5NSP
151800120709
151900120709     C     KEYAR51       KLIST
152000120709     C                   KFLD                    AR5AAS
152100120709     C                   KFLD                    AR5LNP
152200120709     C                   KFLD                    AR5NRS
152300120709     C                   KFLD                    AR5NSP
152400120709     C                   KFLD                    AR5TRD
152500060919     C*
152600060919     C* CHIAVE LETTURA TITAI01L
152700060919     C     KEYtai        KLIST
152800060919     C                   KFLD                    taiAAS
152900060919     C                   KFLD                    taiLNP
153000060919     C                   KFLD                    taiNRS
153100060919     C                   KFLD                    taiNSP
153200060919     C                   KFLD                    taiTBL
153300081103
153400081103     C* CHIAVE LETTURA TITAI01L
153500081103     C     KEYrab        KLIST
153600081103     C                   KFLD                    rabAAS
153700081103     C                   KFLD                    rabLNP
153800081103     C                   KFLD                    rabNRS
153900081103     C                   KFLD                    rabNSP
154000061122
154100061122     C* CHIAVE LETTURA Fiapd01l
154200061122     C     KEYapd        KLIST
154300061122     C                   KFLD                    trcapd
154400061122     C                   KFLD                    taspdc
154500040823      *
154600040823     C     kbol          klist
154700040823     C                   kfld                    tasaas
154800040823     C                   kfld                    taslnp
154900040823     C                   kfld                    tasnrs
155000040823     C                   kfld                    tasnsp
155100080312      *
155200080312     C     klad          klist
155300080312     C                   kfld                    LADAAS
155400080312     C                   kfld                    LADLNP
155500080312     C                   kfld                    LADNRS
155600080312     C                   kfld                    LADNSP
155700000327     C*
155800131029     C     Kevb2f        KLIST
155900131029     C                   KFLD                    EVBAAS
156000131029     C                   KFLD                    EVBLNP
156100131029     C                   KFLD                    EVBNRS
156200131029     C                   KFLD                    EVBNSP
156300131029     C                   KFLD                    EVBDTV
156400131029     C                   KFLD                    EVBORV
156500131029     C                   KFLD                    EVBCEV
156600000327     C* CALCOLA LA DATA CORRENTE
156700000327     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
156800000327     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
156900000327     C                   Z-ADD     N8            G08DAT
157000000327     C                   Z-ADD     *ZEROS        G08INV
157100000327     C                   MOVEL     '0'           G08ERR
157200000327     C                   CALL      'XSRDA8'
157300000327     C                   PARM                    WLBDA8
157400000327     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
157500930208     C*
157600930208     C* IMPOSTO A ZERO IL FINE PGM
157700930208     C                   MOVEL     '0'           FINL00            1
157800931105     C                   MOVEL     *BLANKS       RSUT             20
157900040922     C                   Z-ADD     *ZEROS        NUMblp            9 0
158000040922     C                   Z-ADD     *ZEROS        NUMarb            9 0
158100950308     C                   Z-ADD     *ZEROS        NUMTAS            9 0
158200120709     C**!!!              Z-ADD     *ZEROS        NUMlag            9 0
158300950308     C                   Z-ADD     *ZEROS        NUMTA7            9 0
158400990802     C                   Z-ADD     *ZEROS        NUMTAA            9 0
158500990802     C                   Z-ADD     *ZEROS        NUMTAT            9 0
158600990802     C                   Z-ADD     *ZEROS        NUMTA4            9 0
158700010903     C                   Z-ADD     *ZEROS        NUMTAH            9 0
158800950307     C                   Z-ADD     *ZEROS        NUMCET            9 0
158900950308     C                   Z-ADD     *ZEROS        NUMCED            9 0
159000980325     C                   Z-ADD     *ZEROS        NUMFIM            9 0
159100970904     C                   Z-ADD     *ZEROS        NUMECT            9 0
159200970904     C                   Z-ADD     *ZEROS        NUMECD            9 0
159300000327     C                   Z-ADD     *ZEROS        NUMEVB            9 0
159400000404     C                   Z-ADD     *ZEROS        NUMGCP            9 0
159500000404     C                   Z-ADD     *ZEROS        NUMGAP            9 0
159600000404     C                   Z-ADD     *ZEROS        NUMGNP            9 0
159700001019     C                   Z-ADD     *ZEROS        NUMNON            9 0
159800040303     C                   Z-ADD     *ZEROS        NUMAR5            9 0
159900060919     C                   Z-ADD     *ZEROS        NUMtai            9 0
160000081103     C                   Z-ADD     *ZEROS        NUMrab            9 0
160100061122     C                   Z-ADD     *ZEROS        NUMsto            9 0
160200021021     C                   Z-ADD     *ZEROS        CNTGCP            9 0
160300021021     C                   Z-ADD     *ZEROS        CNTDCT            9 0
160400021021     C                   Z-ADD     *ZEROS        CNTCSB            9 0
160500040823      * Apro il Fnblp di filiale
160600040823     c                   Clear                   comman
160700040823     c                   Movel(p)  cmd(1)        comman
160800040823     c                   Eval      %Subst(comman:13:8) = 'FNBLP01L'
160900040910      *test sistema informativo di prova
161000040910     c                   if        %subst(knsif:7:1) = 'P'
161100040910     c                   Eval      %Subst(comman:30:10) = wlibp
161200040910     c                   else
161300040910     c                   Eval      %Subst(comman:30:10) = wlib
161400040910     c                   endif
161500040910
161600040823     c                   Eval      comman =
161700040823     c                             %trim(comman) + '/FNBLP01L)'
161800040823     c                   Call      'QCMDEXC'                            99
161900040823     c                   Parm                    comman
162000040823     c                   Parm                    lenght
162100040823     c                   If        *In99
162200040823     c                   seton                                        lr
162300040823     c                   Else
162400040823     c                   Open      Fnblp01l
162500040823     c                   EndIf
162600040823      *
162700040823      * Apro  Fnarb di filiale
162800040823     c                   Clear                   comman
162900040823     c                   Movel(p)  cmd(1)        comman
163000040823     c                   Eval      %Subst(comman:13:8) = 'FNARB01L'
163100040910      *test sistema informativo di prova
163200040910     c                   if        %subst(knsif:7:1) = 'P'
163300040910     c                   Eval      %Subst(comman:30:10) = wlibp
163400040910     c                   else
163500040910     c                   Eval      %Subst(comman:30:10) = wlib
163600040910     c                   endif
163700040910
163800040823     c                   Eval      comman =
163900040823     c                             %trim(comman) + '/FNARB01L)'
164000040823     c                   Call      'QCMDEXC'                            99
164100040823     c                   Parm                    comman
164200040823     c                   Parm                    lenght
164300040823     c                   If        *In99
164400040823     c                   seton                                        lr
164500040823     c                   Else
164600040823     c                   Open      Fnarb01l
164700040823     c                   EndIf
164800040823      *
164900931115     C*
165000950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
165100080925     c* carico i tipi bolla RECUPERO per evitare di salvarli nel TITASS0F
165200081103     c                   clear                   conta
165300080926     c                   open      tabel00f
165400080925     c     keytab1       setll     tabel00f
165500080925     c                   do        *hival
165600080925     c     keytab1       reade(n)  tabel00f
165700080925     c                   if        %eof(tabel00f)
165800080925     c                   leave
165900080925     c                   end
166000080925     c                   movel(p)  tbluni        dstb
166100080925     c                   if        §TBRBL ='R'
166200080925     c                   add       1             conta             2 0
166300080925     c                   movel     tblkey        tbl(conta)
166400080925     c                   end
166500080925     c                   enddo
166600080926     c                   close     tabel00f
166700000000     C*
166800000000     C                   ENDSR
166900940912     O*------------------------------------------------------------------------*
167000940905     OPRTF198   E            TESTA            02
167100950309     O                                            8 'TNSB38R '
167200931105     O                       RSUT                45
167300990802     O                                           82 'TRASF. TITASP -> SALVAT.'
167400931115     O                                           90 'DAL:'
167500950309     O                       PARDTI              99
167600931115     O                                          110 'AL:'
167700950309     O                       PARDTF             119
167800020220     O                                          128 '/PT'
167900020220     O                       PARDFP             132
168000931105     O                                          150 'Data:'
168100950717     O                       DSDAT6             159 '  /  /  '
168200931105     O                                          167 'Ora:'
168300950717     O                       DTORA6             178 '  .  .  '
168400931105     O                                          190 'Pagina:'
168500931105     O                       PAGE          Z    198
168600931105     O          E            TESTA       1  1
168700931105     O                       TES(2)              66
168800931105     O                       TES(3)             132
168900931105     O                       TES(4)             198
169000931105     O          E            TESTA          1
169100931105     O                       TES(1)              66
169200931105     O                       TES(1)             132
169300931105     O                       TES(1)             198
169400001019     O*------------------------------------
169500001019     O          E            ERRDCM         1
169600001019     O                       TASAAS            +  5
169700001019     O                       TASLNP            +  1
169800001019     O                       TASNRS            +  1
169900001019     O                       TASNSP            +  1
170000001019     O                       TASTBL            +  1
170100001019     O                                         +  1 'BOLLA NON CONS.    '
170200931105     O*------------------------------------
170300950307     O          E            ERRTAS         1
170400950307     O                       TASAAS            +  5
170500950307     O                       TASLNP            +  1
170600950307     O                       TASNRS            +  1
170700950307     O                       TASNSP            +  1
170800950307     O                       TASTBL            +  1
170900990802     O                                         +  1 'ERR. WRITE TITAS   '
171000950307     O*------------------------------------
171100950307     O          E            ERRTA7         1
171200950307     O                       TA7AAS            +  5
171300950307     O                       TA7LNP            +  1
171400950307     O                       TA7NRS            +  1
171500950307     O                       TA7NSP            +  1
171600950307     O                       TA7TBL            +  1
171700950307     O                       TA7SVN            +  1
171800990802     O                                         +  1 'ERR. WRITE TITA7   '
171900990802     O*------------------------------------
172000990802     O          E            ERRTAA         1
172100990802     O                       TAAAAS            +  5
172200990802     O                       TAALNP            +  1
172300990802     O                       TAANRS            +  1
172400990802     O                       TAANSP            +  1
172500990802     O                       TAATRC            +  1
172600990802     O                                         +  1 'ERR. WRITE TITAA   '
172700990802     O*------------------------------------
172800990802     O          E            ERRTAT         1
172900990802     O                       TATAAS            +  5
173000990802     O                       TATLNP            +  1
173100990802     O                       TATNRS            +  1
173200990802     O                       TATNSP            +  1
173300990802     O                       TATNSC            +  1
173400990802     O                                         +  1 'ERR. WRITE TITAT   '
173500990802     O*------------------------------------
173600990802     O          E            ERRTA4         1
173700990802     O                       TA4AAS            +  5
173800990802     O                       TA4LNP            +  1
173900990802     O                       TA4NRS            +  1
174000990802     O                       TA4NSP            +  1
174100990802     O                       TA4TRC            +  1
174200990802     O                                         +  1 'ERR. WRITE TITA4   '
174300010903     O*------------------------------------
174400010903     O          E            ERRTAH         1
174500010903     O                       TAHAAS            +  5
174600010903     O                       TAHLNP            +  1
174700010903     O                       TAHNRS            +  1
174800010903     O                       TAHNSP            +  1
174900010903     O                       TAHTRC            +  1
175000010903     O                                         +  1 'ERR. WRITE TITAH   '
175100950307     O*------------------------------------
175200950307     O          E            ERRCET         1
175300950307     O                       CETAAS            +  5
175400950307     O                       CETLNP            +  1
175500950307     O                       CETNRS            +  1
175600950307     O                       CETNSP            +  1
175700950307     O                       CETTBL            +  1
175800950309     O                                         +  1 'ERR. WRITE ECCET   '
175900950307     O*------------------------------------
176000950307     O          E            ERRCED         1
176100950307     O                       CEDAAS            +  5
176200950307     O                       CEDLNP            +  1
176300950307     O                       CEDNRS            +  1
176400950307     O                       CEDNSP            +  1
176500950307     O                       CEDTBL            +  1
176600950307     O                       CEDCMP            +  1
176700950309     O                                         +  1 'ERR. WRITE ECCED   '
176800980325     O*------------------------------------
176900980325     O          E            ERRFIM         1
177000980325     O                       FIMAAS            +  5
177100980325     O                       FIMLNP            +  1
177200980325     O                       FIMNRS            +  1
177300980325     O                       FIMNSP            +  1
177400980325     O                       FIMTBL            +  1
177500980325     O                                         +  1 'ERR. WRITE ECFIM   '
177600970903     O*------------------------------------
177700970903     O          E            ERRECT         1
177800970903     O                       ECTNBC        Z   +  5
177900970903     O                       ECTCMP        Z   +  1
178000970903     O                       ECTDDC            +  1 '    /  /  '
178100970903     O                                         +  1 'ERR. WRITE ECECT   '
178200970903     O*------------------------------------
178300970903     O          E            ERRECD         1
178400970903     O                       ECDNBC        Z   +  5
178500970903     O                       ECDCMP        Z   +  1
178600970903     O                       ECDFRA            +  1
178700970903     O                       ECDIOE            +  1
178800970903     O                       ECDKCC        Z   +  1
178900970903     O                       ECDKSC        Z   +  1
179000970903     O                       ECDRIC            +  1
179100970903     O                       ECDCCF        Z   +  1
179200970903     O                       ECDDDO            +  1 '    /  /  '
179300970903     O                                         +  1 'ERR. WRITE ECECD   '
179400000404     O*------------------------------------
179500000404     O          E            ERREVB         1
179600000404     O                       EVBAAS            +  5
179700000404     O                       EVBLNP            +  1
179800000404     O                       EVBNRS            +  1
179900000404     O                       EVBNSP            +  1
180000000404     O                                         +  1 'ERR. WRITE FNEVB   '
180100000404     O*------------------------------------
180200000404     O          E            ERRGCP         1
180300000404     O                       GCPAAS            +  5
180400000404     O                       GCPLNP            +  1
180500000404     O                       GCPNRS            +  1
180600000404     O                       GCPNSP            +  1
180700050301     O                                         +  1 'ERR. WRITE TIGCP   '
180800050303     O          E            ERRGCP1        1
180900050303     O                       GCPAAS            +  5
181000050303     O                       GCPLNP            +  1
181100050303     O                       GCPNRS            +  1
181200050303     O                       GCPNSP            +  1
181300050303     O                                         +  1 'GIACENZA ANCORA IN FILIALE'
181400000404     O*------------------------------------
181500000404     O          E            ERRGAP         1
181600000404     O                       GAPAAS            +  5
181700000404     O                       GAPLNP            +  1
181800000404     O                       GAPNRS            +  1
181900000404     O                       GAPNSP            +  1
182000050301     O                                         +  1 'ERR. WRITE TIGAP   '
182100000404     O*------------------------------------
182200000404     O          E            ERRGNP         1
182300000404     O                       GNPAGC            +  5
182400000404     O                       GNPFGC            +  4
182500000404     O                       GNPNGC            +  1
182600050301     O                                         +  1 'ERR. WRITE TIGNP   '
182700040303     O*------------------------------------
182800040303     O          E            ERRAR5         1
182900040303     O                       AR5AAS            +  5
183000040303     O                       AR5LNP            +  1
183100040303     O                       AR5NRS            +  1
183200040303     O                       AR5NSP            +  1
183300040303     O                                         +  1 'ERR. WRITE FIAR5   '
183400060919     O          E            ERRtai         1
183500060919     O                       taiAAS            +  5
183600060919     O                       taiLNP            +  1
183700060919     O                       taiNRS            +  1
183800060919     O                       taiNSP            +  1
183900060919     O                       taitbl            +  1
184000060919     O                                         +  1 'ERR. WRITE TITAI   '
184100081103     O          E            ERRrab         1
184200081103     O                       rabAAS            +  5
184300081103     O                       rabLNP            +  1
184400081103     O                       rabNRS            +  1
184500081103     O                       rabNSP            +  1
184600081103     O                                         +  1 'ERR. WRITE FIRAB   '
184700950309     O*------------------------------------
184800000404     O          E            NOCCSB         1
184900950309     O                       CSBAAS            +  5
185000950309     O                       CSBLNP            +  1
185100950309     O                       CSBNRS            +  1
185200950309     O                       CSBNSP            +  1
185300000404     O                                         +  1 'BOLLA NON CANC:TNCSB'
185400000404     O*------------------------------------
185500040715     O*****     E            NOCDCT         1
185600040715     O*****                  DCTAAS            +  5
185700040715     O*****                  DCTLNP            +  1
185800040715     O*****                  DCTNRS            +  1
185900040715     O*****                  DCTNSP            +  1
186000040715     O*****                                    +  1 'BOLLA NON CANC:FNDCT'
186100040715     O          E            NOCDCT         1
186200040715     O                       i12AAS            +  5
186300040715     O                       i12LNP            +  1
186400040715     O                       i12NRS            +  1
186500040715     O                       i12NSP            +  1
186600040715     O                                         +  1 'BOLLA NON CANC:FNDCT'
186700000404     O*------------------------------------
186800000404     O          E            NOCGCP         1
186900000404     O                       GCPAAS            +  5
187000000404     O                       GCPLNP            +  1
187100000404     O                       GCPNRS            +  1
187200000404     O                       GCPNSP            +  1
187300050301     O                                         +  1 'BOLLA NON CANC:TIGCP'
187400040922     O*------------------------------------
187500040922     O          E            ERRblp         1
187600040922     O                       blpAAS            +  5
187700040922     O                       blpLNP            +  1
187800040922     O                       blpNRS            +  1
187900040922     O                       blpNSP            +  1
188000040922     O                                         +  1 'ERR. Trovato reK su FNBLP'
188100040922     O*------------------------------------
188200040922     O          E            ERRarb         1
188300040922     O                       arbAAS            +  5
188400040922     O                       arbLNP            +  1
188500040922     O                       arbNRS            +  1
188600040922     O                       arbNSP            +  1
188700040922     O                                         +  1 'ERR. Trovato reK su FNARB'
188800061122     O          E            ERRSTO         1
188900061122     O                       TASAAS            +  5
189000061122     O                       TASLNP            +  1
189100061122     O                       TASNRS            +  1
189200061122     O                       TASNSP            +  1
189300061122     O                       TASTBL            +  1
189400061122     O                                         +  1 'ERR. WRITE STORICO '
189500040922     O*------------------------------------
189600950308     O          E            STAFIN         1
189700990802     O                                           30 'N.REC.TRASF. TITAS'
189800950308     O                       NUMTAS        2     44
189900950308     O          E            STAFIN         1
190000990802     O                                           30 'N.REC.TRASF. TITA7'
190100950308     O                       NUMTA7        2     44
190200990802     O          E            STAFIN         1
190300990802     O                                           30 'N.REC.TRASF. TITAA'
190400990802     O                       NUMTAA        2     44
190500990802     O          E            STAFIN         1
190600990802     O                                           30 'N.REC.TRASF. TITAT'
190700990802     O                       NUMTAT        2     44
190800990802     O          E            STAFIN         1
190900990802     O                                           30 'N.REC.TRASF. TITA4'
191000990802     O                       NUMTA4        2     44
191100010903     O          E            STAFIN         1
191200010903     O                                           30 'N.REC.TRASF. TITAH'
191300010903     O                       NUMTAH        2     44
191400950308     O          E            STAFIN         1
191500950308     O                                           30 'N.REC.TRASF. ECCET'
191600950308     O                       NUMCET        2     44
191700950308     O          E            STAFIN         1
191800950308     O                                           30 'N.REC.TRASF. ECCED'
191900950308     O                       NUMCED        2     44
192000980325     O          E            STAFIN         1
192100980325     O                                           30 'N.REC.TRASF. ECFIM'
192200980325     O                       NUMFIM        2     44
192300970903     O          E            STAFIN         1
192400970903     O                                           30 'N.REC.TRASF. ECECT'
192500970903     O                       NUMECT        2     44
192600970903     O          E            STAFIN         1
192700970903     O                                           30 'N.REC.TRASF. ECECD'
192800970903     O                       NUMECD        2     44
192900000327     O          E            STAFIN         1
193000000327     O                                           30 'N.REC.TRASF. FNEVB'
193100000327     O                       NUMEVB        2     44
193200000404     O          E            STAFIN         1
193300050301     O                                           30 'N.REC.TRASF. TIGCP'
193400000404     O                       NUMGCP        2     44
193500000404     O          E            STAFIN         1
193600050301     O                                           30 'N.REC.TRASF. TIGAP'
193700000404     O                       NUMGAP        2     44
193800000404     O          E            STAFIN         1
193900050301     O                                           30 'N.REC.TRASF. TIGNP'
194000000404     O                       NUMGNP        2     44
194100040310     O          E            STAFIN         1
194200040310     O                                           30 'N.REC. BOLLE FIAR5'
194300040310     O                       NUMAR5        2     44
194400060919     O          E            STAFIN         1
194500060919     O                                           30 'N.REC. BOLLE TITAI'
194600060919     O                       NUMTAI        2     44
194700081103     O          E            STAFIN         1
194800081103     O                                           30 'N.REC. BOLLE FIRAB'
194900081103     O                       NUMRAB        2     44
195000061122     O          E            STAFIN         1
195100061122     O                                           30 'N.BOLLE IN STORICO'
195200061122     O                       NUMSTO        2     44
195300001019     O          E            STAFIN         1
195400001020     O                                           30 'N.REC. BOLLE SCART'
195500001019     O                       NUMNON        2     44
195600021021     O          E            STAFIN         1
195700021021     O                                           30 'DI CUI:'
195800021021     O          E            STAFIN         1
195900021021     O                                           30 'X GIACENZA APERTA '
196000021021     O                       CNTGCP        2     44
196100021021     O          E            STAFIN         1
196200021021     O                                           30 'X DANNO PENDENTE  '
196300021021     O                       CNTDCT        2     44
196400021021     O          E            STAFIN         1
196500021021     O                                           30 'X C/A ATTIVO      '
196600021021     O                       CNTCSB        2     44
196700040922     O          E            STAFIN         1
196800040922     O                                           30 'N.REK.Trovati FNBLP'
196900040922     O                       NUMblp        2     44
197000040922     O          E            STAFIN         1
197100040922     O                                           30 'N.REK.Trovati FNARB'
197200040922     O                       NUMarb        2     44
197300060626     O          E            STAFIN         1
197400060626     O                                           +0 'Chiusuara pgm: FINTAS'
197500060626     O                       fintas              +1
197600060626     O                       tasdsp              +1
197700060626     O                       dsdatf              +1
197800060626     O               98                          +1 'SHTDN'
197900060626     O                                           +1 'FINL00'
198000060626     O                       finl00              +1
198100950308     O          E            STAFIN           60
198200931115     O                                           76 '*** FINE STAMPA ***'
198300931115     O                                          150 'Data:'
198400950717     O                       DSDAT6             159 '  /  /  '
198500990802     O                                          167 'Ora:'
198600950717     O                       DTORA6             178 '  :  :  '
198700940912     O*------------------------------------------------------------------------*
198800970903     O* AGGIORNAMENTO FILES
198900970903     O*------------------------------------------------------------------------*
199000970903     OECECT     E            AGGECT
199100970903     O                       ECTATB
199200970903     O                       ECTFTR
199300970903     OECECD     E            AGGECD
199400970903     O                       ECDATB
199500970903     O                       ECDFTR
199600970903     O*------------------------------------------------------------------------*
199700040823** cmd
199800040823OVRDBF FILE(        ) TOFILE(
199900931105**  TES - RIGHE IN STAMPA
200000931105------------------------------------------------------------------  1
200100931105 DATA    ORA       AA LNP SE N.SPED. TB     ERR  DESCRIZIONE ERROR  2
200200950308E                                                                   3
200300931105                                                                    4
200400931105                                                                    5
200500931105                                                                    6
200600931105                                                                    7
200700931105                                                                    8
200800931105                                                                    9
200900931105                                                                   10
201000931105                                                                   11
201100931105                                                                   12
201200931105                                                                   13
201300931105                                                                   14
201400931105                                                                   15
