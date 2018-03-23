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
026900160927      * RCD FAT FIAR5
027000160927     d dAR5fat       e ds
027100120709
027200940912     C*------------------------------------------------------------------------*
027300940912     C*?MAIN LINE
027400940912     C*------------------------------------------------------------------------*
027500000000     C*
027600930129     C* ROUTINE INIZIALE
027700930129     C                   EXSR      RUTINZ
027800931105     C*
027900931105     C* STAMPA RIGA DI TESTATA
028000931105     C                   EXCEPT    TESTA
028100930601     C*
028200930601     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
028300930601     C     FINL00        IFEQ      '0'                                          --- 1 -->
028400000000     C*
028500990802     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA SU TITAS
028600950307     C                   EXSR      SETTAS
028700930128     C*
028800990802     C* CICLO FINO A FINE FILE TITAS OPPURE
028900950308     C* RICHIESTA CHIUSURA PGM OPPURE
029000950308     C* FINE PGM PER ERRORE
029100950308     C                   SETOFF                                       98
029200950307     C     FINTAS        DOWEQ     '0'                                          --- 2 -->
029300950308     C     *IN98         ANDEQ     '0'
029400950308     C     FINL00        ANDEQ     '0'
029500080214      * verifico   che non esistano ARB e BLP altrimenti non cancello
029600040823      * e leggo un'altro rekord
029700040823     C     Kbol          Chain     fnblp01l                           33
029800040922     C  N33              ADD       1             NUMblp                         *REC TRASF TITA7
029900040922     C  n33              EXCEPT    ERRblp
030000080214     C     kbol          Chain     fnarb01l                           34
030100080214     C  N34              ADD       1             NUMarb                         *REC TRASF TITA7
030200080214     C  n34              EXCEPT    ERRarb
030300080214     c                   if        not *in33 or not *in34
030400080214     c                   goto      altrorek
030500080214     c                   end
030600950307     C*
030700001019     C                   WRITE     TITASS00                             99      *SCRIVO TITASS
030800001019     C  N99              DELETE    TITASP00                             99      *CANCELLO TITASP
030900950307     C   99              EXCEPT    ERRTAS
031000950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
031100990802     C  N99              ADD       1             NUMTAS                         *REC TRASF TITAS
031200050705     c* se bolla ha immagine di laguna scrivo record sul file TILAD00F
031300050705     c                   if        not *in99
031400050705     c                   movel     tasflo        dtasflo
031500050705     c                   if        §floiml <> ' '
031600050705     c                   exsr      srlaguna
031700050705     c                   end
031800120709       //?se la bolla ha le immagini dei documenti firmati dal cliente da
031900120709       //?tornare al mittente scrivo nuovo record sul file TIJDD00F
032000120709      /free
032100120709        AR5trd = 'DOC';
032200120709        chain (TASaas:TASlnp:TASnrs:TASnsp:AR5trd) FIAR531C;
032300120709        IF  %found(FIAR531C);
032400120709          dAR5doc = AR5uni;
032500120709          IF  §AR5jfid <> *blanks;
032600120709            exsr srJdoc;
032700120709          ENDIF;
032800120709        ENDIF;
032900120709      /end-free
033000050705     c                   end
033100950307     C*
033200990802     C* SE ESISTE TITA7 TRASFERISCO ANCHE QUELLO
033300950307     C     TASSV3        IFNE      *BLANKS                                      --- 3 -->
033400990802     C                   EXSR      SETTA7                                       *PRIMO TITA7
033500950307     C     FINTA7        DOWEQ     '0'                                          --- 4 -->
033600990802     C                   WRITE     TITA7S00                             99      *SCRIVO TITA7S
033700001019     C  N99              DELETE    TITA730C                             99      *CANCELLO TITA730C
033800950307     C   99              EXCEPT    ERRTA7
033900950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
034000990802     C  N99              ADD       1             NUMTA7                         *REC TRASF TITA7
034100990802     C                   EXSR      LETTA7                                       *SUCCESSIVO TITA7
034200950307     C                   ENDDO                                                  <-- 4 ---
034300950307     C                   ENDIF                                                  <-- 3 ---
034400990802     C*
034500990802     C* TRASFERISCE TITAA - ANAGRAFICHE
034600990802     C                   EXSR      SETTAA                                       *PRIMO TITAA
034700990802DO  3C     FINTAA        DOWEQ     '0'
034800990802     C                   WRITE     TITAAS00                             99      *SCRIVO TITAAS
034900001019     C  N99              DELETE    TITAA30C                             99      *CANCELLO TITAA30C
035000990802     C   99              EXCEPT    ERRTAA
035100990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
035200990802     C  N99              ADD       1             NUMTAA                         *REC TRASF TITAA
035300990802     C                   EXSR      LETTAA                                       *SUCCESSIVO TITAA
035400990802E   3C                   ENDDO
035500061121     C* verifico se scritto i dati anagrafici del mittente altrimenti aggancio anagrafica
035600061121     c                   if        tasrsm = *blanks
035700061121     c                   exsr      recana
035800061121     c                   endif
035900990802     C*
036000990802     C* TRASFERISCE TITAT - SEGNACOLLI
036100990802     C                   EXSR      SETTAT                                       *PRIMO TITAT
036200990802DO  3C     FINTAT        DOWEQ     '0'
036300990802     C                   WRITE     TITATS00                             99      *SCRIVO TITATS
036400001019     C  N99              DELETE    TITAT30C                             99      *CANCELLO TITAT30C
036500990802     C   99              EXCEPT    ERRTAT
036600990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
036700990802     C  N99              ADD       1             NUMTAT                         *REC TRASF TITAT
036800990802     C                   EXSR      LETTAT                                       *SUCCESSIVO TITAT
036900990802E   3C                   ENDDO
037000990802     C*
037100990802     C* TRASFERISCE TITA4 - RIFERIMENTI
037200990802     C                   EXSR      SETTA4                                       *PRIMO TITA4
037300990802DO  3C     FINTA4        DOWEQ     '0'
037400990802     C                   WRITE     TITA4S00                             99      *SCRIVO TITA4S
037500001019     C  N99              DELETE    TITA430C                             99      *CANCELLO TITA430C
037600990802     C   99              EXCEPT    ERRTA4
037700990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
037800990802     C  N99              ADD       1             NUMTA4                         *REC TRASF TITA4
037900990802     C                   EXSR      LETTA4                                       *SUCCESSIVO TITA4
038000990802E   3C                   ENDDO
038100010903     C*
038200010903     C* TRASFERISCE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
038300010903     C                   EXSR      SETTAH                                       *PRIMO TITAH
038400010903DO  3C     FINTAH        DOWEQ     '0'
038500010903     C                   WRITE     TITAHS00                             99      *SCRIVO TITAHS
038600010903     C  N99              DELETE    TITAH30C                             99      *CANCELLO TITAH30C
038700010903     C   99              EXCEPT    ERRTAH
038800010903     C   99              MOVEL     '1'           FINL00                         *FINE PGM
038900010903     C  N99              ADD       1             NUMTAH                         *REC TRASF TITAH
039000010903     C                   EXSR      LETTAH                                       *SUCCESSIVO TITAH
039100010903E   3C                   ENDDO
039200950307     C*
039300950307     C* SE ESISTE ECCET LO TRASFERISCE
039400971008     C                   Z-ADD     999           MINNBC                         MIMIMO BLOCCO
039500950307     C                   Z-ADD     TASAAS        CETAAS                         ANNO SPEDIZIONE
039600950307     C                   Z-ADD     TASLNP        CETLNP                         LINEA DI PARTENZA
039700950307     C                   Z-ADD     TASNRS        CETNRS                         NUMERO SERIE
039800950307     C                   Z-ADD     TASNSP        CETNSP                         NUMERO SPEDIZIONE
039900950307     C                   MOVEL     TASTBL        CETTBL                         TIPO BOLLA
040000950307     C     KEYCET        CHAIN     ECCET30C                           99
040100950307     C     *IN99         IFEQ      *OFF                                         --- 3 --> *TROVATO
040200970903     C*
040300970903     C* MEMORIZZA IL NUMERO BLOCCO COMPETENZE TESTATA PIU' PICCOLO
040400971008IF  4C     CETNBC        IFGT      *ZEROS
040500971008IF  5C     CETNBC        IFLT      MINNBC
040600970903     C                   Z-ADD     CETNBC        MINNBC
040700971008E   5C                   ENDIF
040800971008E   4C                   ENDIF
040900970903     C*
041000950309     C                   WRITE     ECCETS                               99      *SCRIVO ECCETS
041100001019     C  N99              DELETE    ECCET30C                             99      *CANCELLO ECCET30C
041200950307     C   99              EXCEPT    ERRCET
041300950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
041400950308     C  N99              ADD       1             NUMCET                         *REC TRASF ECCET
041500950307     C*
041600950307     C* SE ESISTE ECCED TRASFERISCO ANCHE QUELLO
041700950307     C     CETDET        IFEQ      'S'                                          --- 4 -->
041800950307     C                   EXSR      SETCED                                       *PRIMO ECCED
041900950307     C     FINCED        DOWEQ     '0'                                          --- 5 -->
042000950309     C                   WRITE     ECCEDS                               99      *SCRIVO ECCEDS
042100001020     C  N99              DELETE    ECCED30C                             99      *CANCELLO ECCED30C
042200950307     C   99              EXCEPT    ERRCED
042300950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
042400950308     C  N99              ADD       1             NUMCED                         *REC TRASF ECCED
042500950307     C                   EXSR      LETCED                                       *SUCCESSIVO ECCED
042600950307     C                   ENDDO                                                  <-- 5 ---
042700950307     C                   ENDIF                                                  <-- 4 ---
042800950307     C                   ENDIF                                                  <-- 3 ---
042900980325     C*
043000980325     C* SE ESISTE ECFIM LO TRASFERISCE
043100980325     C                   Z-ADD     TASAAS        FIMAAS                         ANNO SPEDIZIONE
043200980325     C                   Z-ADD     TASLNP        FIMLNP                         LINEA DI PARTENZA
043300980325     C                   Z-ADD     TASNRS        FIMNRS                         NUMERO SERIE
043400980325     C                   Z-ADD     TASNSP        FIMNSP                         NUMERO SPEDIZIONE
043500980325     C                   MOVEL     TASTBL        FIMTBL                         TIPO BOLLA
043600001019     C     KEYFIM        CHAIN     ECFIM30C                           99
043700980325     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
043800980325     C                   WRITE     ECFIMS00                             99      *SCRIVE
043900001019     C  N99              DELETE    ECFIM30C                             99      *CANCELLA
044000980325     C   99              EXCEPT    ERRFIM
044100980325     C   99              MOVEL     '1'           FINL00                         *FINE PGM
044200980325     C  N99              ADD       1             NUMFIM                         *REC TRASF ECCET
044300011127     C     KEYFIM        READE     ECFIM30C                               99
044400980325     C                   ENDDO                                                  <-- 3 ---
044500000327     C*
044600000327     C* SE ESISTE FNEVB LO TRASFERISCE
044700000327     C                   Z-ADD     TASAAS        EVBAAS                         ANNO SPEDIZIONE
044800000327     C                   Z-ADD     TASLNP        EVBLNP                         LINEA DI PARTENZA
044900000327     C                   Z-ADD     TASNRS        EVBNRS                         NUMERO SERIE
045000000327     C                   Z-ADD     TASNSP        EVBNSP                         NUMERO SPEDIZIONE
045100001019     C     KEYEVB        CHAIN     FNEVB01L                           99
045200000327     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
045300000327     C                   WRITE     FNEVBS                               99      *SCRIVE
045400001019     C  N99              DELETE    FNEVB01L                             99      *CANCELLA
045500131029     c                   if        not *in99
045600131029     C     Kevb2f        chain     FNEVB22L
045700131029     c                   if        %found(fnevb22l)
045800131029     c                   delete    fnevb22l
045900131029     c                   endif
046000131029     c                   endif
046100000327     C   99              EXCEPT    ERREVB
046200000327     C   99              MOVEL     '1'           FINL00                         *FINE PGM
046300000327     C  N99              ADD       1             NUMEVB                         *REC TRASF FNEVB
046400000327     C     KEYEVB        READE     FNEVB01L                               99
046500000327     C                   ENDDO                                                  <-- 3 ---
046600000404     C*
046700050301     C* SE ESISTE TIGCP LO TRASFERISCE
046800001018     C     TASFGC        IFEQ      'S'
046900000404     C                   Z-ADD     TASAAS        KCPAAS                         ANNO SPEDIZIONE
047000000404     C                   Z-ADD     TASLNP        KCPLNP                         LINEA DI PARTENZA
047100000404     C                   Z-ADD     TASNRS        KCPNRS                         NUMERO SERIE
047200000404     C                   Z-ADD     TASNSP        KCPNSP                         NUMERO SPEDIZIONE
047300050301     C     KE2GCP        CHAIN     TIGCP51L                           99
047400001018     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
047500050303     c     gcpsede       ifeq      'S'
047600050301     C                   WRITE     TIGCPS                               99      *SCRIVE
047700050301     C  N99              DELETE    TIGCP51L                             99      *CANCELLA
047800000404     C   99              EXCEPT    ERRGCP
047900000404     C   99              MOVEL     '1'           FINL00                         *FINE PGM
048000000404     C  N99              ADD       1             NUMGCP                         *REC TRASF FNEVB
048100000404     C*
048200050301     C* SE ESISTE TIGAP LO TRASFERISCE
048300000404     C                   Z-ADD     GCPAAS        KAPAAS                         ANNO SPEDIZIONE
048400000404     C                   Z-ADD     GCPLNP        KAPLNP                         LINEA DI PARTENZA
048500000404     C                   Z-ADD     GCPNRS        KAPNRS                         NUMERO SERIE
048600000404     C                   Z-ADD     GCPNSP        KAPNSP                         NUMERO SPEDIZIONE
048700050301     C     KEYGAP        CHAIN     TIGAP01L                           99
048800001018     C     *IN99         DOWEQ     *OFF
048900050301     C                   WRITE     TIGAPS                               99      *SCRIVE
049000050301     C  N99              DELETE    TIGAP01L                             99      *CANCELLA
049100001017     C   99              EXCEPT    ERRGAP
049200001017     C   99              MOVEL     '1'           FINL00                         *FINE PGM
049300001017     C  N99              ADD       1             NUMGAP                         *REC TRASF FNEVB
049400050301     C     KEYGAP        READE     TIGAP01L                               99
049500001018     C                   ENDDO
049600000404     C*
049700050301     C* SE ESISTE TIGNP LO TRASFERISCE
049800000404     C                   Z-ADD     GCPAGC        KNPAGC                         ANNO APERTURA
049900000404     C                   Z-ADD     GCPFGC        KNPFGC                         FILIALE APERTURA
050000000404     C                   Z-ADD     GCPNGC        KNPNGC                         NUMERO GIACENZA
050100050301     C     KEYGNP        CHAIN     TIGNP01L                           99
050200001018     C     *IN99         DOWEQ     *OFF
050300050301     C                   WRITE     TIGNPS                               99      *SCRIVE
050400050301     C  N99              DELETE    TIGNP01L                             99      *CANCELLA
050500001017     C   99              EXCEPT    ERRGNP
050600001017     C   99              MOVEL     '1'           FINL00                         *FINE PGM
050700001017     C  N99              ADD       1             NUMGNP                         *REC TRASF FNEVB
050800050301     C     KEYGNP        READE     TIGNP01L                               99
050900001018     C                   ENDDO
051000050303     c                   else
051100050303     C                   EXCEPT    ERRGCP1
051200060707     C*                  MOVEL     '1'           FINL00                         *FINE PGM
051300050303     C                   ENDIF
051400000404     C*
051500000404     C* LETTURA SUCCESSIVA GIACENZE
051600050301     C     KE2GCP        READE     TIGCP51L                               99
051700001018     C                   ENDDO
051800001018     C                   ENDIF
051900040303     C*
052000040303     C* SE ESISTE FIAR5 LO TRASFERISCE
052100040303     C                   Z-ADD     TASAAS        AR5AAS                         ANNO SPEDIZIONE
052200040303     C                   Z-ADD     TASLNP        AR5LNP                         LINEA DI PARTENZA
052300040303     C                   Z-ADD     TASNRS        AR5NRS                         NUMERO SERIE
052400040303     C                   Z-ADD     TASNSP        AR5NSP                         NUMERO SPEDIZIONE
052500160927     c                   clear                   dar5fat
052600040531     C     KEYAR5        CHAIN     FIAR531C                           99
052700040303     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
052800160927      *se trovo rekord FAT mi salvo i dati
052900160927     c                   if        ar5trd = 'FAT'
053000160927     c                   movel     ar5uni        dar5fat
053100160927     c                   endif
053200040303     C                   WRITE     FIAR5S                               99      *SCRIVE
053300040531     C  N99              DELETE    FIAR531C                             99      *CANCELLA
053400040303     C   99              EXCEPT    ERRAR5
053500040303     C   99              MOVEL     '1'           FINL00                         *FINE PGM
053600040303     C  N99              ADD       1             NUMAR5                         *REC TRASF FNEVB
053700040531     C     KEYAR5        READE     FIAR531C                               99
053800040303     C                   ENDDO                                                  <-- 3 ---
053900060919     C*
054000060919     C* SE ESISTE TITAI LO TRASFERISCE
054100060919     C                   Z-ADD     TASAAS        taiAAS                         ANNO SPEDIZIONE
054200060919     C                   Z-ADD     TASLNP        taiLNP                         LINEA DI PARTENZA
054300060919     C                   Z-ADD     TASNRS        taiNRS                         NUMERO SERIE
054400060919     C                   Z-ADD     TASNSP        taiNSP                         NUMERO SPEDIZIONE
054500060919     C                   movel     TASTBL        taiTBL                         NUMERO SPEDIZIONE
054600060919     C     KEYtai        CHAIN     titai01l                           99
054700060919     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
054800060919     C                   WRITE     titaiS                               99      *SCRIVE
054900060919     C  N99              DELETE    titai01l                             99      *CANCELLA
055000060919     C   99              EXCEPT    ERRtai
055100060919     C   99              MOVEL     '1'           FINL00                         *FINE PGM
055200060919     C  N99              ADD       1             NUMtai                         *REC TRASF FNEVB
055300060919     C     KEYtai        READE     titai01l                               99
055400060919     C                   ENDDO                                                  <-- 3 ---
055500081103     C* SE ESISTE FIRAB LO TRASFERISCE
055600081103     C                   Z-ADD     TASAAS        rabAAS                         ANNO SPEDIZIONE
055700081103     C                   Z-ADD     TASLNP        rabLNP                         LINEA DI PARTENZA
055800081103     C                   Z-ADD     TASNRS        rabNRS                         NUMERO SERIE
055900081103     C                   Z-ADD     TASNSP        rabNSP                         NUMERO SPEDIZIONE
056000081103     C     KEYrab        CHAIN     firab01l                           99
056100081103     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
056200081103     C                   WRITE     firabS                               99      *SCRIVE
056300081103     C  N99              DELETE    firab01l                             99      *CANCELLA
056400081103     C   99              EXCEPT    ERRrab
056500081103     C   99              MOVEL     '1'           FINL00                         *FINE PGM
056600081103     C  N99              ADD       1             NUMrab                         *REC TRASF FNEVB
056700081103     C     KEYrab        READE     firab01l                               99
056800081103     C                   ENDDO                                                  <-- 3 ---
056900061214      * decodifico alcuni dati per il nuovo storico  e lo scrivo se non bolla Poste
057000061214     c                   If        tastsp <> 'P'
057100080926     c* se bolla di recupero non scrivo storico
057200080926     c                   z-add     1             conta
057300080926     c     tastbl        lookup    tbl(conta)                             44
057400080926     c                   if        not *in44
057500061122     c                   EXSR      wrttassto
057600061214     c                   endif
057700080926     c                   endif
057800061122
057900000000     C*
058000980325     C* LETTURA SUCCESSIVA BOLLE
058100040823     c     altrorek      tag
058200040823
058300060707     c                   if        finl00 = '0'
058400990802     C                   EXSR      LETTAS                                       *SUCCESSIVO TITAS
058500060707     c                   end
058600950308     C                   SHTDN                                        98        *CONTR.CHIUSURA PGM
058700930128     C*
058800950307     C                   ENDDO                                                  <-- 2 ---
058900950307     C                   ENDIF                                                  <-- 1 ---
059000970903     C*
059100970903     C* PULISCE I CODICI COMPETENZA -TESTATA E DETTAGLIO- CON NUMERO BLOCCO
059200970903     C* MINORE DI QUELLO PIU' PICCOLO CON CUI SONO STATE CALCOLATE LE BOLLE
059300971008IF  1C     MINNBC        IFNE      999
059400970903     C     *LOVAL        SETLL     ECECT01L
059500970903     C                   READ      ECECT01L                               97
059600971008DO  2C     *IN97         DOWEQ     *OFF
059700971008IF  3C     ECTATB        IFEQ      *BLANKS
059800971008IF  4C     ECTNBC        IFLT      MINNBC
059900970903     C                   MOVEL     'A'           ECTATB
060000970903     C                   MOVEL     *BLANKS       ECTFTR
060100970903     C                   EXCEPT    AGGECT                                       *AGGIORNAMENTO CAMPI
060200970903     C                   WRITE     ECECTS                               97      *SCRIVO ECECTS
060300970903     C   97              EXCEPT    ERRECT                                       *SCRIVE ERRORE
060400970903     C   97              MOVEL     '1'           FINL00                         *FINE PGM
060500970903     C  N97              ADD       1             NUMECT                         *REC TRASF ECECT
060600971008E   4C                   ENDIF
060700971008E   3C                   ENDIF
060800970903     C                   READ      ECECT01L                               97
060900971008E   2C                   ENDDO
061000950309     C*
061100970903     C     *LOVAL        SETLL     ECECD01L
061200970903     C                   READ      ECECD01L                               97
061300971008DO  2C     *IN97         DOWEQ     *OFF
061400971008IF  3C     ECDATB        IFEQ      *BLANKS
061500971008IF  4C     ECDNBC        IFLT      MINNBC
061600970903     C                   MOVEL     'A'           ECDATB
061700970903     C                   MOVEL     *BLANKS       ECDFTR
061800970903     C                   EXCEPT    AGGECD                                       *AGGIORNAMENTO CAMPI
061900970903     C                   WRITE     ECECDS                               97      *SCRIVO ECECDS
062000970903     C   97              EXCEPT    ERRECD                                       *SCRIVE ERRORE
062100970903     C   97              MOVEL     '1'           FINL00                         *FINE PGM
062200970903     C  N97              ADD       1             NUMECD                         *REC TRASF ECECD
062300971008E   4C                   ENDIF
062400971008E   3C                   ENDIF
062500970903     C                   READ      ECECD01L                               97
062600971008E   2C                   ENDDO
062700971008E   1C                   ENDIF
062800970903     C*
062900950309     C* AGGIORNA LA TABELLA CON L'ULTIMA DATA DI TRASFERIM. (SOLO SE TUTTO OK)
063000950309     C     *IN98         IFEQ      '0'
063100950309     C     FINL00        ANDEQ     '0'
063200950309     C                   EXSR      AGGTAB
063300950309     C                   ENDIF
063400931105     C*
063500931105     C* STAMPA RIGA DI FINE STAMPA
063600950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
063700931105     C                   EXCEPT    STAFIN
063800050705     c* chiudo il pgm di archiviazione Laguna
063900050705     C                   CLEAR                   Tnsb34ds
064000050705     C                   MOVEL     'C'           sb34tla
064100050705     C                   CALL      'TNSB34R'
064200050705     C                   PARM                    TNSB34ds
064300000000     C*
064400000000     C                   SETON                                        LR
064500940912     C*------------------------------------------------------------------------*
064600990802     C*? SETTAS - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAS
064700940912     C*------------------------------------------------------------------------*
064800950307     C     SETTAS        BEGSR
064900931115     C*
065000931115     C* AZZERO CONTATORE RECORD LETTI
065100950307     C                   MOVEL     '0'           FINTAS            1
065200930208     C*
065300930316     C* POSIZIONAMENTO E PRIMA LETTURA
065400941123     C                   Z-ADD     PARDTI        DSDATI
065500941123     C                   Z-ADD     PARDTF        DSDATF
065600020220     C                   Z-ADD     PARDFP        DSDATFP
065700941123     C                   Z-ADD     DSAASI        TASAAS
065800941123     C                   Z-ADD     DSMGSI        TASMGS
065900990802     C     KEYTAS        SETLL     TITAS38C                           99        *FINE-ARCHIV
066000930625     C*
066100930625     C     *IN99         IFEQ      '1'
066200950307     C                   MOVEL     '1'           FINTAS
066300930625     C                   ELSE
066400950307     C                   EXSR      LETTAS
066500001020     C                   ENDIF
066600930208     C*
066700930208     C                   ENDSR
066800940912     C*------------------------------------------------------------------------*
066900990802     C*? LETTAS - LETTURA PROSSIMO RECORD TITAS
067000940912     C*------------------------------------------------------------------------*
067100950307     C     LETTAS        BEGSR
067200930625     C*
067300930625     C* LEGGO FINO A:
067400930625     C*     - FINE FILE O SUPERATO LIMITE FINALE
067500930625     C*     - TROVATO RECORD VALIDO
067600930625     C                   MOVEL     'N'           WRECOK
067700950307     C     FINTAS        DOWEQ     '0'                                          --- 1 -->
067800930625     C     WRECOK        ANDEQ     'N'
067900061121      * pulisco lo storico
068000061121     c                   clear                   titassto
068100930208     C*
068200930316     C* LETTURE SUCCESSIVE ALLA PRIMA
068300990802     C                   READ      TITAS38C                               99
068400960413     C*
068500960413     C     *IN99         IFEQ      '1'                                          *FINE FILE
068600020220     C     TASDSP        ORGT      DSDATF                                       *SUPERATA DATA FINE
068700950307     C                   MOVEL     '1'           FINTAS
068800930625     C                   ELSE
068900950309     C                   EXSR      CHKTAS                                       *CONTR.VALIDITA REC.
069000001019     C                   ENDIF
069100930625     C*
069200001019     C                   ENDDO                                                  <-- 1 ---
069300930208     C*
069400930208     C                   ENDSR
069500950307     C*------------------------------------------------------------------------*
069600990802     C*? SETTA7 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA7
069700950307     C*------------------------------------------------------------------------*
069800950307     C     SETTA7        BEGSR
069900950307     C*
070000950307     C* AZZERO CONTATORE RECORD LETTI
070100950307     C                   MOVEL     '0'           FINTA7            1
070200950307     C*
070300950307     C* POSIZIONAMENTO E PRIMA LETTURA
070400950307     C                   Z-ADD     TASAAS        TA7AAS
070500950307     C                   Z-ADD     TASLNP        TA7LNP
070600950307     C                   Z-ADD     TASNRS        TA7NRS
070700950307     C                   Z-ADD     TASNSP        TA7NSP
070800950307     C                   MOVEL     TASTBL        TA7TBL
070900001019     C*
071000001019     C     KEYTA7        CHAIN     TITA730C                           99        *FINE-ARCHIV
071100950307     C     *IN99         IFEQ      '1'
071200950307     C                   MOVEL     '1'           FINTA7
071300001019     C                   ENDIF
071400950307     C*
071500950307     C                   ENDSR
071600950307     C*------------------------------------------------------------------------*
071700990802     C*? LETTA7 - LETTURA PROSSIMO RECORD TITA7
071800950307     C*------------------------------------------------------------------------*
071900001019     C     LETTA7        BEGSR
072000001020     C*
072100001020     C* LETTURE SUCCESSIVE ALLA PRIMA
072200010124     C                   READ      TITA730C                               99
072300950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
072400010124     C     TA7AAS        ORNE      TASAAS
072500010124     C     TA7LNP        ORNE      TASLNP
072600010124     C     TA7NRS        ORNE      TASNRS
072700010124     C     TA7NSP        ORNE      TASNSP
072800010124     C     TA7TBL        ORNE      TASTBL
072900950307     C                   MOVEL     '1'           FINTA7
073000001019     C                   ENDIF
073100950307     C*
073200950307     C                   ENDSR
073300990802     C*------------------------------------------------------------------------*
073400990802     C*? SETTAA - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAA - ANAGRAFICHE
073500990802     C*------------------------------------------------------------------------*
073600990802     C     SETTAA        BEGSR
073700990802     C*
073800990802     C* AZZERO CONTATORE RECORD LETTI
073900990802     C                   MOVEL     '0'           FINTAA            1
074000990802     C*
074100990802     C* POSIZIONAMENTO E PRIMA LETTURA
074200990802     C                   Z-ADD     TASAAS        TAAAAS
074300990802     C                   Z-ADD     TASLNP        TAALNP
074400990802     C                   Z-ADD     TASNRS        TAANRS
074500990802     C                   Z-ADD     TASNSP        TAANSP
074600001019     C     KEYTAA        CHAIN     TITAA30C                           99        *FINE-ARCHIV
074700990802     C     *IN99         IFEQ      '1'
074800990802     C                   MOVEL     '1'           FINTAA
074900061121      * recupero dati anagrafici del mittente
075000061121     c                   else
075100061121     c                   exsr      rectaa
075200001019     C                   ENDIF
075300990802     C*
075400990802     C                   ENDSR
075500990802     C*------------------------------------------------------------------------*
075600990802     C*? LETTAA - LETTURA PROSSIMO RECORD TITAA - ANAGRAFICHE
075700990802     C*------------------------------------------------------------------------*
075800990802     C     LETTAA        BEGSR
075900010124     C*
076000010124     C                   READ      TITAA30C                               99
076100990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
076200010130     C     TAAAAS        ORNE      TASAAS
076300010130     C     TAALNP        ORNE      TASLNP
076400010130     C     TAANRS        ORNE      TASNRS
076500010130     C     TAANSP        ORNE      TASNSP
076600990802     C                   MOVEL     '1'           FINTAA
076700001019     C                   ENDIF
076800061121      * se letto
076900061123     c                   if        fintaa = '0'
077000061121     c                   exsr      rectaa
077100061121     c                   endif
077200990802     C*
077300990802     C                   ENDSR
077400061121     C*------------------------------------------------------------------------*
077500061121     C*? RECTAA - RECUPERO DATI ANAGRAFICI MITTENTE
077600061121     C*------------------------------------------------------------------------*
077700061121     C     RECTAA        BEGSR
077800061121      * dati anagrafici mittente
077900061121     c                   IF        taatrc = 'M'
078000061121     c                   eval      tasrsm = taarsc
078100061121     c                   eval      tasinm = taaind
078200061121     c                   eval      tascam = taacap
078300061121     c                   eval      taslom = taaloc
078400061121     c                   eval      tasprm = taaprv
078500061121     c                   eval      tasnzm = taanaz
078600061122      * iva  per i franchi non coidficati
078700061122     c                   if        %subst(tastbl:1:1) = 'F'
078800061122     c                   eval      tasiva = taacpi
078900061122     c                   endif
079000061122
079100061121     c                   endif
079200061121
079300061121     c                   endsr
079400061121     C*------------------------------------------------------------------------*
079500061121     C*? RECANA - RECUPERO DA ANAGRAFICO CLIENTE
079600061121     C*------------------------------------------------------------------------*
079700061121     C     RECANA        BEGSR
079800061121     C                   clear                   ds_CNACO
079900061121     C                   clear                   ds_CNIND
080000061121     C                   clear                   ds_CNCLP
080100061121     C                   clear                   ds_FNCLS
080200061121     c                   z-add     TASccm        I69kac
080300061121     c                   z-add     TASccm        I69kin
080400061121     c                   call      'TIBS69R'
080500061121     c                   parm                    TIBS69ds
080600061121     c                   parm                    ds_CNACO
080700061121     c                   parm                    ds_CNIND
080800061121     c                   parm                    ds_CNCLP
080900061121     c                   parm                    ds_FNCLS
081000061122if  3c                   if        O69err <> *on
081100061122     c                   movel     ACOrag        TASrsm
081200061122     c                   movel     INDvia        TASinm
081300061122     c                   movel     INDcap        TAScam
081400061122     c                   movel     INDcit        TASlom
081500061122     c                   movel     INDprv        TASprm
081600061122     c                   movel     INDsta        TASnzm
081700061122e   3c                   endif
081800061121
081900061121     c                   endsr
082000061122     C*------------------------------------------------------------------------*
082100061122     C*? RECIVA - RECUPERO DA ANAGRAFICO CLIENTE IVA E/O CODICE FISCALE
082200061122     C*------------------------------------------------------------------------*
082300061122     C     RECIVA        BEGSR
082400061122     C                   clear                   ds_CNACO
082500061122     C                   clear                   ds_CNIND
082600061122     C                   clear                   ds_CNCLP
082700061122     C                   clear                   ds_FNCLS
082800061122     c                   z-add     TASKSC        I69kin
082900061122     c                   call      'TIBS69R'
083000061122     c                   parm                    TIBS69ds
083100061122     c                   parm                    ds_CNACO
083200061122     c                   parm                    ds_CNIND
083300061122     c                   parm                    ds_CNCLP
083400061122     c                   parm                    ds_FNCLS
083500061122
083600061122     c                   endsr
083700990802     C*------------------------------------------------------------------------*
083800990802     C*? SETTAT - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAT - SEGNACOLLI
083900990802     C*------------------------------------------------------------------------*
084000990802     C     SETTAT        BEGSR
084100990802     C*
084200990802     C* AZZERO CONTATORE RECORD LETTI
084300990802     C                   MOVEL     '0'           FINTAT            1
084400990802     C*
084500990802     C* POSIZIONAMENTO E PRIMA LETTURA
084600990802     C                   Z-ADD     TASAAS        TATAAS
084700990802     C                   Z-ADD     TASLNP        TATLNP
084800990802     C                   Z-ADD     TASNRS        TATNRS
084900990802     C                   Z-ADD     TASNSP        TATNSP
085000001019     C     KEYTAT        CHAIN     TITAT30C                           99        *FINE-ARCHIV
085100990802     C     *IN99         IFEQ      '1'
085200990802     C                   MOVEL     '1'           FINTAT
085300001019     C                   ENDIF
085400990802     C*
085500990802     C                   ENDSR
085600990802     C*------------------------------------------------------------------------*
085700990802     C*? LETTAT - LETTURA PROSSIMO RECORD TITAT - SEGNACOLLI
085800990802     C*------------------------------------------------------------------------*
085900990802     C     LETTAT        BEGSR
086000010124     C*
086100010124     C                   READ      TITAT30C                               99
086200990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
086300010124     C     TATAAS        ORNE      TASAAS
086400010124     C     TATLNP        ORNE      TASLNP
086500010124     C     TATNRS        ORNE      TASNRS
086600010124     C     TATNSP        ORNE      TASNSP
086700990802     C                   MOVEL     '1'           FINTAT
086800001019     C                   ENDIF
086900990802     C*
087000990802     C                   ENDSR
087100990802     C*------------------------------------------------------------------------*
087200990802     C*? SETTA4 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA4 - RIFERIMENTI
087300990802     C*------------------------------------------------------------------------*
087400990802     C     SETTA4        BEGSR
087500990802     C*
087600990802     C* AZZERO CONTATORE RECORD LETTI
087700990802     C                   MOVEL     '0'           FINTA4            1
087800990802     C*
087900990802     C* POSIZIONAMENTO E PRIMA LETTURA
088000990802     C                   Z-ADD     TASAAS        TA4AAS
088100990802     C                   Z-ADD     TASLNP        TA4LNP
088200990802     C                   Z-ADD     TASNRS        TA4NRS
088300990802     C                   Z-ADD     TASNSP        TA4NSP
088400001019     C     KEYTA4        CHAIN     TITA430C                           99        *FINE-ARCHIV
088500990802     C     *IN99         IFEQ      '1'
088600990802     C                   MOVEL     '1'           FINTA4
088700160412      * recupero riferimento mittente alfanumerico e natura merce
088800061122     c                   else
088900061122     c                   exsr      recta4
089000001019     C                   ENDIF
089100990802     C*
089200990802     C                   ENDSR
089300990802     C*------------------------------------------------------------------------*
089400990802     C*? LETTA4 - LETTURA PROSSIMO RECORD TITA4 - RIFERIMENTI
089500990802     C*------------------------------------------------------------------------*
089600990802     C     LETTA4        BEGSR
089700010124     C*
089800010124     C                   READ      TITA430C                               99
089900990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
090000010124     C     TA4AAS        ORNE      TASAAS
090100010124     C     TA4LNP        ORNE      TASLNP
090200010124     C     TA4NRS        ORNE      TASNRS
090300010124     C     TA4NSP        ORNE      TASNSP
090400990802     C                   MOVEL     '1'           FINTA4
090500001019     C                   ENDIF
090600061122      * se letto
090700061123     c                   if        finta4 = '0'
090800061122     c                   exsr      recta4
090900061122     c                   endif
091000990802     C*
091100990802     C                   ENDSR
091200061122     C*------------------------------------------------------------------------*
091300061122     C*? RECTA4 - RECUPERO RIFERIMENTO MITTENTE ALFANUMERICO / CODICE FISCALE
091400061122     C*------------------------------------------------------------------------*
091500061122     C     RECTA4        BEGSR
091600061122      * riferimento mittente alfanumerico
091700061122     c                   IF        ta4trc = 'A'
091800061122     c                   movel     ta4not        dta4a
091900061122     c                   movel     §ta4aRMA      Tasrma
092000160412     c                   movel     §ta4anas      Tasnas
092100061122     c                   endif
092200061122      * CODICE FISCALE
092300061122     c                   IF        ta4trc = 'Q'
092400061122     c                   movel     ta4not        dta4q
092500061122      * VERIFICO SE FRANCO O ASSEGNATO
092600061122     c                   If        %subst(tastbl:1:1) = 'F'
092700061122     c                   movel     §TA4QCFM      Tascdf
092800061122     c                   endif
092900061122     c                   If        %subst(tastbl:1:1) = 'A'
093000061122     c                   movel     §TA4QCFD      Tascdf
093100061122     c                   endif
093200061122     c                   endif
093300061122
093400061122     c                   endsr
093500010903     C*------------------------------------------------------------------------*
093600010903     C*? SETTAH - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
093700010903     C*------------------------------------------------------------------------*
093800010903     C     SETTAH        BEGSR
093900010903     C*
094000010903     C* AZZERO CONTATORE RECORD LETTI
094100010903     C                   MOVEL     '0'           FINTAH            1
094200010903     C*
094300010903     C* POSIZIONAMENTO E PRIMA LETTURA
094400010903     C                   Z-ADD     TASAAS        TAHAAS
094500010903     C                   Z-ADD     TASLNP        TAHLNP
094600010903     C                   Z-ADD     TASNRS        TAHNRS
094700010903     C                   Z-ADD     TASNSP        TAHNSP
094800010903     C     KEYTAH        CHAIN     TITAH30C                           99        *FINE-ARCHIV
094900010903     C     *IN99         IFEQ      '1'
095000010903     C                   MOVEL     '1'           FINTAH
095100010903     C                   ENDIF
095200010903     C*
095300010903     C                   ENDSR
095400010903     C*------------------------------------------------------------------------*
095500010903     C*? LETTAH - LETTURA PROSSIMO RECORD TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
095600010903     C*------------------------------------------------------------------------*
095700010903     C     LETTAH        BEGSR
095800010903     C*
095900010903     C                   READ      TITAH30C                               99
096000010903     C     *IN99         IFEQ      '1'                                          *FINE FILE
096100010903     C     TAHAAS        ORNE      TASAAS
096200010903     C     TAHLNP        ORNE      TASLNP
096300010903     C     TAHNRS        ORNE      TASNRS
096400010903     C     TAHNSP        ORNE      TASNSP
096500010903     C                   MOVEL     '1'           FINTAH
096600010903     C                   ENDIF
096700010903     C*
096800010903     C                   ENDSR
096900950307     C*------------------------------------------------------------------------*
097000950307     C*? SETCED - POSIZIONAMENTO E PRIMA LETTURA DEL FILE ECCED
097100950307     C*------------------------------------------------------------------------*
097200950307     C     SETCED        BEGSR
097300950307     C*
097400950307     C* AZZERO CONTATORE RECORD LETTI
097500950307     C                   MOVEL     '0'           FINCED            1
097600950307     C*
097700950307     C* POSIZIONAMENTO E PRIMA LETTURA
097800950307     C                   Z-ADD     TASAAS        CEDAAS
097900950307     C                   Z-ADD     TASLNP        CEDLNP
098000950307     C                   Z-ADD     TASNRS        CEDNRS
098100950307     C                   Z-ADD     TASNSP        CEDNSP
098200950307     C                   MOVEL     TASTBL        CEDTBL
098300001019     C     KEYCED        CHAIN     ECCED30C                           99        *FINE-ARCHIV
098400950307     C     *IN99         IFEQ      '1'
098500950307     C                   MOVEL     '1'           FINCED
098600001019     C                   ENDIF
098700950307     C*
098800950307     C                   ENDSR
098900950307     C*------------------------------------------------------------------------*
099000950307     C*? LETCED - LETTURA PROSSIMO RECORD ECCED
099100950307     C*------------------------------------------------------------------------*
099200950307     C     LETCED        BEGSR
099300950307     C*
099400010124     C                   READ      ECCED30C                               99
099500950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
099600010124     C     CEDAAS        ORNE      TASAAS
099700010124     C     CEDLNP        ORNE      TASLNP
099800010124     C     CEDNRS        ORNE      TASNRS
099900010124     C     CEDNSP        ORNE      TASNSP
100000010124     C     CEDTBL        ORNE      TASTBL
100100950307     C                   MOVEL     '1'           FINCED
100200001019     C                   ENDIF
100300950307     C*
100400950307     C                   ENDSR
100500950307     C*------------------------------------------------------------------------*
100600990802     C*? CHKTAS - CONTROLLA VALIDITA' DEL RECORD DI TITAS
100700950307     C*------------------------------------------------------------------------*
100800950309     C     CHKTAS        BEGSR
100900950307     C*
101000950307     C                   MOVEL     'S'           WRECOK            1
101100000329     C*
101200001023     C* CONTROLLA LO STATO DELLA CONSEGNA IN RELAZIONE AL TIPO BOLLA
101300001023     C* ==> SE TRATTASI DI BOLLA DI RECUPERO (F3/A3/AP) STORICIZZO COMUNQUE
101400001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
101500001018     C     TASDCM        IFEQ      *ZEROS                                       *NON CONSEGNATA
101600001023     C     TASTBL        ANDNE     'F3'
101700001023     C     TASTBL        ANDNE     'A3'
101800001023     C     TASTBL        ANDNE     'AP'
101900010222     C     TASCBO        ANDNE     'E '
102000010222     C     TASCBO        ANDNE     'B '
102100001019     C                   EXCEPT    ERRDCM
102200000329     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
102300001018     C                   ENDIF
102400001018     C                   ENDIF
102500950309     C*
102600950309     C* CONTROLLA IL CONTRASSEGNO
102700001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
102800950309     C     TASFCA        IFEQ      'S'                                          --- 1 -->
102900950309     C                   Z-ADD     TASAAS        CSBAAS
103000950309     C                   Z-ADD     TASLNP        CSBLNP
103100950309     C                   Z-ADD     TASNRS        CSBNRS
103200950309     C                   Z-ADD     TASNSP        CSBNSP
103300950309     C     KEYCSB        CHAIN     TNCSB03L                           99
103400950309     C     *IN99         IFEQ      '0'                                          --- 2 -->
103500950309     C     CSBSTA        IFNE      9                                            --- 3 -->
103600950309     C     CSBBNA        IFEQ      *ZEROS                                       *NON ANCORA PAGATO
103700950309     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
103800021021     C***                EXCEPT    NOCCSB                                       *STAMPO RECORD
103900021021     C                   ADD       1             CNTCSB                         *CONTEGGIO RECORD
104000950309     C                   ENDIF
104100950309     C                   ENDIF                                                  <-- 3 ---
104200950309     C                   ENDIF                                                  <-- 2 ---
104300950309     C                   ENDIF                                                  <-- 1 ---
104400001018     C                   ENDIF                                                  <-- 1 ---
104500000327     C*
104600000327     C* CONTROLLA I DANNI
104700001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
104800001018     C     TASFDA        IFEQ      'S'
104900040715      **
105000040715      **  Sostituito con il pgm FIDN12R che restituisce l'informazione
105100040715      **   sui danni fra bolla madre e figlia
105200040715      **
105300040715     C**                 Z-ADD     TASAAS        KCTAAS
105400040715     C**                 Z-ADD     TASLNP        KCTLNP
105500040715     C**                 Z-ADD     TASNRS        KCTNRS
105600040715     C**                 Z-ADD     TASNSP        KCTNSP
105700040715     C**   KEYDCT        CHAIN     FNDCT02L                           99
105800040715      *
105900040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
106000040715      * SIA COME MAMMA CHE COME FIGLIA
106100040715     c                   clear                   fidn12ds
106200040715     c                   eval      i12aas = TASAAS
106300040715     c                   eval      i12lnp = TASLNP
106400040715     c                   eval      i12nrs = TASNRS
106500040715     c                   eval      i12nsp = TASNSP
106600040715     c                   eval      i12fel = 'E'
106700040715     c                   call      'FIDN12R'
106800040715     c                   parm                    fidn12ds
106900040715      **
107000040715      * se c'è almeno una C.A. aperta
107100040715      **
107200040715     c     o12nca        ifgt      0
107300040715     C**   *IN99         IFEQ      *OFF                                         *TROVATO DANNO
107400040715      **
107500000327     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
107600021021     C***                EXCEPT    NOCDCT                                       *STAMPO RECORD
107700021021     C                   ADD       1             CNTDCT                         *CONTEGGIO RECORD
107800001018     C                   ENDIF
107900001018     C                   ENDIF
108000001018     C                   ENDIF
108100000404     C*
108200000404     C* CONTROLLA LE GIACENZE
108300001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
108400001018     C     TASFGC        IFEQ      'S'
108500000404     C                   Z-ADD     TASAAS        KCPAAS
108600000404     C                   Z-ADD     TASLNP        KCPLNP
108700000404     C                   Z-ADD     TASNRS        KCPNRS
108800000404     C                   Z-ADD     TASNSP        KCPNSP
108900000404     C                   Z-ADD     0             KCPFRG                         *PRIMO PRG
109000050301     C     KEYGCP        CHAIN(N)  TIGCP51L                           99
109100001018     C     *IN99         IFEQ      *OFF
109200001018     C     GCPFAS        IFLT      50                                           *FASI APERTE
109300000404     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
109400021021     C***                EXCEPT    NOCGCP                                       *STAMPO RECORD
109500021021     C                   ADD       1             CNTGCP                         *CONTEGGIO RECORD
109600001018     C                   ENDIF
109700001018     C                   ENDIF
109800001018     C                   ENDIF
109900001018     C                   ENDIF
110000011227     C*
110100020220     C* SE BOLLA POSTA CANCELLARE SOLO SE + VECCHIA D 2 ANNI
110200011227     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
110300011227     C     TASTSP        IFEQ      'P'                                          *BOLLA POSTA
110400020220     C     TASDSP        ANDGT     DSDATFP
110500011227     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
110600011227     C                   ENDIF
110700011227     C                   ENDIF
110800001019     C*
110900140109     C* DEVE Controllare: in caso di RESO controlla se la figlia è stata consegnata
111000140109     C*  altrimenti NON deve cancellare la MAMMA.
111100140109     C*
111200131216     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
111300131216     C                   IF        tasCCA = '2' or tasCCA = '6'                 *BOLLA RESO
111400131216     **
111500131223     C* Chiamata metodo GetLastChild
111600131223     c                   exsr      CHKCONS
111700140102     C                   IF         Consegnata_OK <> 'S'
111800140102     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
111900140102     C                   ENDIF
112000131223     **
112100131216     C                   ENDIF
112200131216     C                   ENDIF
112300131216     C*
112400131216     C*
112500131216     C*
112600001019     C* CONTEGGIO BOLLE SCARTATE
112700001019     C     WRECOK        IFEQ      'N'
112800001019     C                   ADD       1             NUMNON
112900001019     C                   ENDIF
113000950307     C*
113100950307     C                   ENDSR
113200131223     C*------------------------------------------------------------------------*
113300131223     C* CHKCONS  - Verifica se bolla effettivamente consegnata
113400131223     C*------------------------------------------------------------------------*
113500131223     C     CHKCONS       BEGSR
113600131223     C*
113700140102     C                   movel     'N'           Consegnata_OK     1
113800131223     C                   movel     *blanks       wEsito1           1
113900131223     C                   movel     *blanks       wEsito2           1
114000131223     C*
114100131223     C                   eval      wAAS = tasAAS
114200131223     C                   eval      wLNP = tasLNP
114300131223     C                   eval      wNRS = tasNRS
114400131223     C                   eval      wNSP = tasNSP
114500131223     C*
114600131223     C* Chiamata metodo GetLblTyp
114700131223     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
114800131223     C                                                wAAS
114900131223     C                                               :wLNP
115000131223     C                                               :wNRS
115100131223     C                                               :wNSP
115200131223     C                                               :pOutLblTyp
115300131223     C                                               :pOutAnnoBO
115400131223     C                                               :pOutLineaParBO
115500131223     C                                               :pOutSerieBO
115600131223     C                                               :pOutNumSpedBO
115700131223     C                                               :pOutDcmBO
115800131223     C                                               :pOutCcaBO
115900131223     C                                               :pOutRblBO))
116000131223     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
116100131223     C                   if        pOutLblTyp <> 'O'
116200131223     C                   eval      wAAS = pOutAnnoBO
116300131223     C                   eval      wLNP = pOutLineaParBO
116400131223     C                   eval      wNRS = pOutSerieBO
116500131223     C                   eval      wNSP = pOutNumSpedBO
116600131223     C                   endif
116700131223     C*
116800131223     C* Chiamata metodo GetLastChild
116900131223     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
117000131223     C                                                wAAS
117100131223     C                                               :wLNP
117200131223     C                                               :wNRS
117300131223     C                                               :wNSP
117400131223     C                                               :pOutAnnoFI
117500131223     C                                               :pOutLineaParFI
117600131223     C                                               :pOutSerieFI
117700131223     C                                               :pOutNumSpedFI
117800131223     C                                               :pOutDcmFI
117900131223     C                                               :pOutCcaFI))
118000131223     C*
118100140102     C* Considerazioni sullo stato di Consegna della bolla
118200140102     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)
118300140102     C                               or
118400131223     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
118500140102     C                   movel     'S'           Consegnata_OK     1
118600131223     C                   endif
118700131223     C*
118800131223     C                   ENDSR
118900950309     C*------------------------------------------------------------------------*
119000950309     C*?AGGTAB - AGGIORNA LA TABELLA '$3' CON ULTIMA DATA TRASF. TAS00 > TAS10
119100950309     C*------------------------------------------------------------------------*
119200950309     C     AGGTAB        BEGSR
119300950309     C*
119400950309     C                   OPEN      TABEL00F
119500950309     C*
119600950309     C                   Z-ADD     1             KTBUTE
119700950309     C                   MOVEL     '$3'          KTBCOD
119800950309     C                   MOVEL     '1'           KTBKEY
119900950309     C     KEYTAB        CHAIN     TABEL00F                           99
120000950309     C     *IN99         IFEQ      *ON                                          *NON TROVATA
120100950309     C                   ELSE                                                   *TROVATA
120200950309     C                   MOVEL     TBLUNI        DS$3
120300950309     C                   Z-ADD     PARDTF        §$3DP0                         *ULT.DATA TRASF P>SA
120400020220     C                   Z-ADD     PARDFP        §$3DPP                         *ULT.DATA TRASF P>SA
120500950309     C                   MOVEL     DS$3          TBLUNI
120600950309     C                   UPDATE    TABEL
120700950309     C                   ENDIF
120800950309     C*
120900950309     C                   CLOSE     TABEL00F
121000950309     C*
121100950309     C                   ENDSR
121200950717     C*------------------------------------------------------------------------*
121300950717     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
121400950717     C*------------------------------------------------------------------------*
121500950717     C     SUBDAT        BEGSR
121600950717     C                   TIME                    DTTIME
121700950717     C                   Z-ADD     DTANN         DSANN
121800950717     C                   Z-ADD     DTMES         DSMES
121900950717     C                   Z-ADD     DTGIO         DSGIO
122000950717     C                   Z-ADD     DTSEC         DSSEC
122100950717     C                   ENDSR
122200940912     C*------------------------------------------------------------------------*
122300050705     C*? funzione per scivere file x Laguna
122400940912     C*------------------------------------------------------------------------*
122500050705     C     srlaguna      BEGSR
122600050705     c                   clear                   tnsb34ds
122700050929     c* ricavo il progressivo da impostare nella directory ogni 450000 bolle
122800050720     c* cancellate cambio directory
122900120709      * 09/07/2012 con Bocchi verificato che il progressivo non serve più
123000120709     C**!!!              ADD       1             NUMlag                         *REC TRASF TITAS
123100120709     c**!!!numlag        div       450000        sb34prg
123200120709     c**!!!              add       1             sb34prg
123300050705     c                   eval      sb34tla = 'R'
123400050705     c                   eval      sb34aas = tasaas
123500050705     c                   eval      sb34lnp = taslnp
123600050705     c                   eval      sb34nrs = tasnrs
123700050705     c                   eval      sb34nsp = tasnsp
123800150123      *
123900150123      ** andando a pulire di 2 mesi in 2 mesi
124000150123      ** occorre introdurre un nuovo ragionamento sul nome della DIRECTORY di LAGUNA
124100150123      ** determinata nel TNSB34R sulla base della DATA passata in "SB34DTP"
124200050706     c                   eval      sb34dtp = pardtf
124300150123      *
124400150302      *  COME PER LAGUNA anche per archiviare i DOCUMENTI
124500150123      *  Se la spedizione non appartiene all'Anno/Mese della data "FINO AL" (PARDTF)
124600150123      *   del range di pulizia passato (pardti - PARDTF = dal-al), si deve
124700150123      **  sottrarre un mese alla data SB34DTP in modo che LAGUNA imposti il nome
124800150123      **  della Directory sul primo mese dei 2 che si sta pulendo.
124900150123     c                   if        TASanme <> PARDTFanme
125000150123     c                   eval      sb34dtp = pardtf_1m
125100150123     c                   end
125200150123      *
125300050706     c                   eval      sb34cop = parcop
125400050705     c                   call      'TNSB34R'
125500050705     c                   parm                    tnsb34ds
125600050705     c                   if        sb34err <> ' '
125700050705     c                   end
125800050705     C                   ENDSR
125900120709     C*------------------------------------------------------------------------*
126000120709     C*? funzione per scivere file x JDOC
126100120709     C*------------------------------------------------------------------------*
126200120709     C     srJdoc        BEGSR
126300120709     c                   clear                   tnsb34ds
126400120709     c                   eval      sb34tla = 'R'
126500120709     c                   eval      sb34aas = tasaas
126600120709     c                   eval      sb34lnp = taslnp
126700120709     c                   eval      sb34nrs = tasnrs
126800120709     c                   eval      sb34nsp = tasnsp
126900120709     c                   eval      sb34dtp = pardtf
127000150302      *
127100150302      ** Prima andando mese x mese NON esisteva il problema:
127200150302      *  Se la spedizione non appartiene all'Anno/Mese della data "FINO AL" (PARDTF)
127300150302      *   del range di pulizia passato (pardti - PARDTF = dal-al), si deve
127400150302      **  sottrarre un mese alla data SB34DTP come fatto x LAGUNA impostando il nome
127500150302      **  della Directory sul primo mese dei 2 che si sta pulendo.
127600150302     c                   if        TASanme <> PARDTFanme
127700150302     c                   eval      sb34dtp = pardtf_1m
127800150302     c                   end
127900150302      *
128000120709     c                   eval      sb34cop = parcop
128100120709     c                   call      'TNSB34R1'
128200120709     c                   parm                    tnsb34ds
128300120709     c                   if        sb34err <> ' '
128400120709     c                   end
128500120709     C                   ENDSR
128600061122     C*------------------------------------------------------------------------*
128700061122     C*? WRTTASSTO - SCRITTURA FILE STORICO TITASS
128800061122     C*------------------------------------------------------------------------*
128900061122     C     WRTTASSTO     BEGSR
129000080312     c* memorizzo directory laguna per sapere dove è stata archiviata la spe
129100080312     c                   clear                   tasdtfp
129200080312     c                   if        §floiml <> ' '
129300080312     c                   move      tasaas        ladaas
129400080312     c                   move      taslnp        ladlnp
129500080312     c                   move      tasnrs        ladnrs
129600080312     c                   move      tasnsp        ladnsp
129700080312     c     klad          chain     tilad01l
129800080312     c                   If        %found(tilad01l)
129900080312     c                   movel     laddir        tasdtfp
130000080312     c                   end
130100080312     c                   end
130200061122      * Decodifica padroncino
130300061122     c                   If        taspdc <> *zeros
130400061122     c     Keyapd        chain     fiapd01l
130500061122     c                   If        %found(fiapd01l)
130600061122     c                   eval      taspdd = apdrsc
130700061122     c                   endif
130800061122     c                   endif
130900061122
131000061122      * recupero c/assegno
131100061122     c                   if        tasfca = 'S'
131200061122     c                   z-add     csbcas        Tascas
131300061122     c                   movel     csbvca        tasvca
131400061122     c                   endif
131500061122
131600061122     c                   move      tasksc        com04
131700061122      * PARTITA IVA / CODICE FISCALE
131800061122     c                   If        %subst(tastbl:1:1) = 'A'  and
131900061122     c                             tascpd <> *blanks
132000061122     c                   eval      tasiva  = tascpd
132100061122     c                   endif
132200061122      * se partita iva non valorizzata
132300061122     c                   If        tasiva = *blanks  or tascdf = *blanks
132400061122      * codificato franco o assegnato
132500061122     c                   if        com04  <>  9999  and com04  <>  8888
132600061122     c                   exsr      reciva
132700061122     c                   if        o69err <> *on
132800061122     c                   if        tascdf = *blanks
132900061122     c                   eval      tasiva = indiva
133000061122     c                   endif
133100061122      * se devo cercare anche il codife fiscale
133200061122     c                   if        tascdf = *blanks
133300061122     c                   eval      tascdf = indcdf
133400061122     c                   endif
133500061122
133600061122     c                   endif
133700061122     c                   endif
133800061122     c                   endif
133900061124      * valorizzo il flag che mi identifica le spedizioni senza segnacollo
134000091229     c                   If        Tasfns <> 'S' and tasncd = *zeros
134100061124     c                   eval      tassss = 'S'
134200061124     c                   endif
134300061122
134400160927      * verifico se la ds dar5fat è valorizzata
134500160927     c                   If        §ar5pktas > 0
134600160927     c                   eval      taspkf = §ar5pktas
134700160927     c                   endif
134800160927
134900160927     c                   If        §ar5vltas > 0
135000160927     c                   eval      tasvlf = §ar5vltas
135100160927     c                   endif
135200160927
135300061122     c                   write     titassto                             99
135400061122     C   99              EXCEPT    ERRsto
135500061122     C   99              MOVEL     '1'           FINL00                         *FINE PGM
135600061122     C  N99              ADD       1             NUMsto                         *REC TRASF FNEVB
135700061122
135800061122     C                   ENDSR
135900061122     C*
136000050705     C*------------------------------------------------------------------------*
136100050705     C*? RUTINZ - ROUTINE INIZIALE
136200050705     C*------------------------------------------------------------------------*
136300050705     C     RUTINZ        BEGSR
136400931115     C*
136500931115     C* PARAMETRI IN INPUT AL PGM
136600931115     C     *ENTRY        PLIST
136700931115     C                   PARM                    KPJBA
136800931115     C                   MOVEL     KPJBU         PARAM
136900950309     C*
137000150123     C*  Avendo cambiato il modo di pulire da mese x mese in due mesi in due mesi
137100150123     C*   devo preparare una data che verrà utilizzata da Laguna che corrisponde
137200150123     C*   alla data finale del range passato meno 1 mese -->  quindi la data di
137300150123     C*   fine mese del mese precedente a quella del range (PARDTF-1m)
137400150128     c                   move      parDTF        Data08            8 0
137500150128     c                   move      '01'          Data08
137600150128     c                   move      Data08        Data_ISO
137700150128      * prende il fine mese precedente e se lo memorizza
137800150128     c                   subdur    1:*DAYS       Data_ISO
137900150123     c                   move      Data_ISO      parDTF_1m
138000150123     C*
138100150128     C*
138200950309     C* DEFINISCO CAMPI
138300950309     C     *LIKE         DEFINE    TBLKUT        KTBUTE
138400950309     C     *LIKE         DEFINE    TBLCOD        KTBCOD
138500950309     C     *LIKE         DEFINE    TBLKEY        KTBKEY
138600970903     C     *LIKE         DEFINE    CETNBC        MINNBC
138700040715     C***  *LIKE         DEFINE    DCTAAS        KCTAAS                         *FNDCT00F
138800040715     C***  *LIKE         DEFINE    DCTLNP        KCTLNP
138900040715     C***  *LIKE         DEFINE    DCTNRS        KCTNRS
139000040715     C***  *LIKE         DEFINE    DCTNSP        KCTNSP
139100050301     C     *LIKE         DEFINE    GCPAAS        KCPAAS                         *TIGCP00F
139200000404     C     *LIKE         DEFINE    GCPLNP        KCPLNP
139300000404     C     *LIKE         DEFINE    GCPNRS        KCPNRS
139400000404     C     *LIKE         DEFINE    GCPNSP        KCPNSP
139500000404     C     *LIKE         DEFINE    GCPFRG        KCPFRG
139600050301     C     *LIKE         DEFINE    GAPAAS        KAPAAS                         *TIGAP00F
139700000404     C     *LIKE         DEFINE    GAPLNP        KAPLNP
139800000404     C     *LIKE         DEFINE    GAPNRS        KAPNRS
139900000404     C     *LIKE         DEFINE    GAPNSP        KAPNSP
140000050301     C     *LIKE         DEFINE    GNPAGC        KNPAGC                         *TIGNP00F
140100000404     C     *LIKE         DEFINE    GNPFGC        KNPFGC
140200000404     C     *LIKE         DEFINE    GNPNGC        KNPNGC
140300950309     C*
140400950309     C* CHIAVE LETTURA TABEL00F - COMPLETA
140500950309     C     KEYTAB        KLIST
140600950309     C                   KFLD                    KTBUTE
140700950309     C                   KFLD                    KTBCOD
140800950309     C                   KFLD                    KTBKEY
140900080925     C     KEYTAB1       KLIST
141000080925     C                   KFLD                    KTBUTE
141100080925     C                   KFLD                    KTBCOD
141200080925     C                   Z-ADD     1             KTBUTE
141300080925     C                   MOVEL     'TB'          KTBCOD
141400950309     C*
141500950309     C* CHIAVE LETTURA TNCSB01L - COMPLETA
141600950309     C     KEYCSB        KLIST
141700950309     C                   KFLD                    CSBAAS
141800950309     C                   KFLD                    CSBLNP
141900950309     C                   KFLD                    CSBNRS
142000950309     C                   KFLD                    CSBNSP
142100000327     C*
142200000327     C* CHIAVE LETTURA FNDCT02L - COMPLETA
142300040715     C***  KEYDCT        KLIST
142400040715     C***                KFLD                    KCTAAS
142500040715     C***                KFLD                    KCTLNP
142600040715     C***                KFLD                    KCTNRS
142700040715     C***                KFLD                    KCTNSP
142800930208     C*
142900990802     C* CHIAVE LETTURA TITAS38C - PARZIALE
143000930621     C     KEYTAS        KLIST
143100941123     C                   KFLD                    TASAAS
143200941123     C                   KFLD                    TASMGS
143300950307     C*
143400990802     C* CHIAVE LETTURA TITA730C - COMPLETA
143500950307     C     KEYTA7        KLIST
143600950307     C                   KFLD                    TA7AAS
143700950307     C                   KFLD                    TA7LNP
143800950307     C                   KFLD                    TA7NRS
143900950307     C                   KFLD                    TA7NSP
144000950307     C                   KFLD                    TA7TBL
144100990802     C*
144200990802     C* CHIAVE LETTURA TITAA30C - COMPLETA
144300990802     C     KEYTAA        KLIST
144400990802     C                   KFLD                    TAAAAS
144500990802     C                   KFLD                    TAALNP
144600990802     C                   KFLD                    TAANRS
144700990802     C                   KFLD                    TAANSP
144800990802     C*
144900990802     C* CHIAVE LETTURA TITAT30C - COMPLETA
145000990802     C     KEYTAT        KLIST
145100990802     C                   KFLD                    TATAAS
145200990802     C                   KFLD                    TATLNP
145300990802     C                   KFLD                    TATNRS
145400990802     C                   KFLD                    TATNSP
145500990802     C*
145600990802     C* CHIAVE LETTURA TITA430C - COMPLETA
145700990802     C     KEYTA4        KLIST
145800990802     C                   KFLD                    TA4AAS
145900990802     C                   KFLD                    TA4LNP
146000990802     C                   KFLD                    TA4NRS
146100990802     C                   KFLD                    TA4NSP
146200010903     C*
146300010903     C* CHIAVE LETTURA TITAH30C - COMPLETA
146400010903     C     KEYTAH        KLIST
146500010903     C                   KFLD                    TAHAAS
146600010903     C                   KFLD                    TAHLNP
146700010903     C                   KFLD                    TAHNRS
146800010903     C                   KFLD                    TAHNSP
146900950209     C*
147000950307     C* CHIAVE LETTURA ECCET30C - COMPLETA
147100950220     C     KEYCET        KLIST
147200950220     C                   KFLD                    CETAAS                         ANNO SPEDIZIONE
147300950220     C                   KFLD                    CETLNP                         LINEA DI PARTENZA
147400950220     C                   KFLD                    CETNRS                         NUMERO SERIE
147500950220     C                   KFLD                    CETNSP                         NUMERO SPEDIZIONE
147600950220     C                   KFLD                    CETTBL                         TIPO BOLLA
147700950307     C*
147800950307     C* CHIAVE LETTURA ECCED30C - COMPLETA
147900950307     C     KEYCED        KLIST
148000950307     C                   KFLD                    CEDAAS                         ANNO SPEDIZIONE
148100950307     C                   KFLD                    CEDLNP                         LINEA DI PARTENZA
148200950307     C                   KFLD                    CEDNRS                         NUMERO SERIE
148300950307     C                   KFLD                    CEDNSP                         NUMERO SPEDIZIONE
148400950307     C                   KFLD                    CEDTBL                         TIPO BOLLA
148500980325     C*
148600980325     C* CHIAVE LETTURA ECFIM30C - COMPLETA
148700980325     C     KEYFIM        KLIST
148800980325     C                   KFLD                    FIMAAS                         ANNO SPEDIZIONE
148900980325     C                   KFLD                    FIMLNP                         LINEA DI PARTENZA
149000980325     C                   KFLD                    FIMNRS                         NUMERO SERIE
149100980325     C                   KFLD                    FIMNSP                         NUMERO SPEDIZIONE
149200980325     C                   KFLD                    FIMTBL                         TIPO BOLLA
149300000327     C*
149400000327     C* CHIAVE LETTURA FNEVB01L - COMPLETA
149500000327     C     KEYEVB        KLIST
149600000327     C                   KFLD                    EVBAAS                         ANNO SPEDIZIONE
149700000327     C                   KFLD                    EVBLNP                         LINEA DI PARTENZA
149800000327     C                   KFLD                    EVBNRS                         NUMERO SERIE
149900000327     C                   KFLD                    EVBNSP                         NUMERO SPEDIZIONE
150000000404     C*
150100050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
150200000404     C     KEYGCP        KLIST
150300000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
150400000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
150500000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
150600000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
150700000404     C                   KFLD                    KCPFRG                         PRG RIAPERTURE
150800000404     C*
150900050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
151000000404     C     KE2GCP        KLIST
151100000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
151200000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
151300000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
151400000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
151500000404     C*
151600050301     C* CHIAVE LETTURA TIGAP01L - PARZIALE
151700000404     C     KEYGAP        KLIST
151800000404     C                   KFLD                    KAPAAS                         ANNO SPEDIZIONE
151900000404     C                   KFLD                    KAPLNP                         LINEA DI PARTENZA
152000000404     C                   KFLD                    KAPNRS                         NUMERO SERIE
152100000404     C                   KFLD                    KAPNSP                         NUMERO SPEDIZIONE
152200000404     C*
152300050301     C* CHIAVE LETTURA TIGNP01L - PARZIALE
152400000404     C     KEYGNP        KLIST
152500000404     C                   KFLD                    KNPAGC                         ANNO APERTURA
152600000404     C                   KFLD                    KNPFGC                         FILILAE APERTURA
152700000404     C                   KFLD                    KNPNGC                         NUMERO GIACENZA
152800040303     C*
152900040531     C* CHIAVE LETTURA FIAR531C
153000040303     C     KEYAR5        KLIST
153100040303     C                   KFLD                    AR5AAS
153200040303     C                   KFLD                    AR5LNP
153300040303     C                   KFLD                    AR5NRS
153400040303     C                   KFLD                    AR5NSP
153500120709
153600120709     C     KEYAR51       KLIST
153700120709     C                   KFLD                    AR5AAS
153800120709     C                   KFLD                    AR5LNP
153900120709     C                   KFLD                    AR5NRS
154000120709     C                   KFLD                    AR5NSP
154100120709     C                   KFLD                    AR5TRD
154200060919     C*
154300060919     C* CHIAVE LETTURA TITAI01L
154400060919     C     KEYtai        KLIST
154500060919     C                   KFLD                    taiAAS
154600060919     C                   KFLD                    taiLNP
154700060919     C                   KFLD                    taiNRS
154800060919     C                   KFLD                    taiNSP
154900060919     C                   KFLD                    taiTBL
155000081103
155100081103     C* CHIAVE LETTURA TITAI01L
155200081103     C     KEYrab        KLIST
155300081103     C                   KFLD                    rabAAS
155400081103     C                   KFLD                    rabLNP
155500081103     C                   KFLD                    rabNRS
155600081103     C                   KFLD                    rabNSP
155700061122
155800061122     C* CHIAVE LETTURA Fiapd01l
155900061122     C     KEYapd        KLIST
156000061122     C                   KFLD                    trcapd
156100061122     C                   KFLD                    taspdc
156200040823      *
156300040823     C     kbol          klist
156400040823     C                   kfld                    tasaas
156500040823     C                   kfld                    taslnp
156600040823     C                   kfld                    tasnrs
156700040823     C                   kfld                    tasnsp
156800080312      *
156900080312     C     klad          klist
157000080312     C                   kfld                    LADAAS
157100080312     C                   kfld                    LADLNP
157200080312     C                   kfld                    LADNRS
157300080312     C                   kfld                    LADNSP
157400000327     C*
157500131029     C     Kevb2f        KLIST
157600131029     C                   KFLD                    EVBAAS
157700131029     C                   KFLD                    EVBLNP
157800131029     C                   KFLD                    EVBNRS
157900131029     C                   KFLD                    EVBNSP
158000131029     C                   KFLD                    EVBDTV
158100131029     C                   KFLD                    EVBORV
158200131029     C                   KFLD                    EVBCEV
158300000327     C* CALCOLA LA DATA CORRENTE
158400000327     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
158500000327     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
158600000327     C                   Z-ADD     N8            G08DAT
158700000327     C                   Z-ADD     *ZEROS        G08INV
158800000327     C                   MOVEL     '0'           G08ERR
158900000327     C                   CALL      'XSRDA8'
159000000327     C                   PARM                    WLBDA8
159100000327     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
159200930208     C*
159300930208     C* IMPOSTO A ZERO IL FINE PGM
159400930208     C                   MOVEL     '0'           FINL00            1
159500931105     C                   MOVEL     *BLANKS       RSUT             20
159600040922     C                   Z-ADD     *ZEROS        NUMblp            9 0
159700040922     C                   Z-ADD     *ZEROS        NUMarb            9 0
159800950308     C                   Z-ADD     *ZEROS        NUMTAS            9 0
159900120709     C**!!!              Z-ADD     *ZEROS        NUMlag            9 0
160000950308     C                   Z-ADD     *ZEROS        NUMTA7            9 0
160100990802     C                   Z-ADD     *ZEROS        NUMTAA            9 0
160200990802     C                   Z-ADD     *ZEROS        NUMTAT            9 0
160300990802     C                   Z-ADD     *ZEROS        NUMTA4            9 0
160400010903     C                   Z-ADD     *ZEROS        NUMTAH            9 0
160500950307     C                   Z-ADD     *ZEROS        NUMCET            9 0
160600950308     C                   Z-ADD     *ZEROS        NUMCED            9 0
160700980325     C                   Z-ADD     *ZEROS        NUMFIM            9 0
160800970904     C                   Z-ADD     *ZEROS        NUMECT            9 0
160900970904     C                   Z-ADD     *ZEROS        NUMECD            9 0
161000000327     C                   Z-ADD     *ZEROS        NUMEVB            9 0
161100000404     C                   Z-ADD     *ZEROS        NUMGCP            9 0
161200000404     C                   Z-ADD     *ZEROS        NUMGAP            9 0
161300000404     C                   Z-ADD     *ZEROS        NUMGNP            9 0
161400001019     C                   Z-ADD     *ZEROS        NUMNON            9 0
161500040303     C                   Z-ADD     *ZEROS        NUMAR5            9 0
161600060919     C                   Z-ADD     *ZEROS        NUMtai            9 0
161700081103     C                   Z-ADD     *ZEROS        NUMrab            9 0
161800061122     C                   Z-ADD     *ZEROS        NUMsto            9 0
161900021021     C                   Z-ADD     *ZEROS        CNTGCP            9 0
162000021021     C                   Z-ADD     *ZEROS        CNTDCT            9 0
162100021021     C                   Z-ADD     *ZEROS        CNTCSB            9 0
162200040823      * Apro il Fnblp di filiale
162300040823     c                   Clear                   comman
162400040823     c                   Movel(p)  cmd(1)        comman
162500040823     c                   Eval      %Subst(comman:13:8) = 'FNBLP01L'
162600040910      *test sistema informativo di prova
162700040910     c                   if        %subst(knsif:7:1) = 'P'
162800040910     c                   Eval      %Subst(comman:30:10) = wlibp
162900040910     c                   else
163000040910     c                   Eval      %Subst(comman:30:10) = wlib
163100040910     c                   endif
163200040910
163300040823     c                   Eval      comman =
163400040823     c                             %trim(comman) + '/FNBLP01L)'
163500040823     c                   Call      'QCMDEXC'                            99
163600040823     c                   Parm                    comman
163700040823     c                   Parm                    lenght
163800040823     c                   If        *In99
163900040823     c                   seton                                        lr
164000040823     c                   Else
164100040823     c                   Open      Fnblp01l
164200040823     c                   EndIf
164300040823      *
164400040823      * Apro  Fnarb di filiale
164500040823     c                   Clear                   comman
164600040823     c                   Movel(p)  cmd(1)        comman
164700040823     c                   Eval      %Subst(comman:13:8) = 'FNARB01L'
164800040910      *test sistema informativo di prova
164900040910     c                   if        %subst(knsif:7:1) = 'P'
165000040910     c                   Eval      %Subst(comman:30:10) = wlibp
165100040910     c                   else
165200040910     c                   Eval      %Subst(comman:30:10) = wlib
165300040910     c                   endif
165400040910
165500040823     c                   Eval      comman =
165600040823     c                             %trim(comman) + '/FNARB01L)'
165700040823     c                   Call      'QCMDEXC'                            99
165800040823     c                   Parm                    comman
165900040823     c                   Parm                    lenght
166000040823     c                   If        *In99
166100040823     c                   seton                                        lr
166200040823     c                   Else
166300040823     c                   Open      Fnarb01l
166400040823     c                   EndIf
166500040823      *
166600931115     C*
166700950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
166800080925     c* carico i tipi bolla RECUPERO per evitare di salvarli nel TITASS0F
166900081103     c                   clear                   conta
167000080926     c                   open      tabel00f
167100080925     c     keytab1       setll     tabel00f
167200080925     c                   do        *hival
167300080925     c     keytab1       reade(n)  tabel00f
167400080925     c                   if        %eof(tabel00f)
167500080925     c                   leave
167600080925     c                   end
167700080925     c                   movel(p)  tbluni        dstb
167800080925     c                   if        §TBRBL ='R'
167900080925     c                   add       1             conta             2 0
168000080925     c                   movel     tblkey        tbl(conta)
168100080925     c                   end
168200080925     c                   enddo
168300080926     c                   close     tabel00f
168400000000     C*
168500000000     C                   ENDSR
168600940912     O*------------------------------------------------------------------------*
168700940905     OPRTF198   E            TESTA            02
168800950309     O                                            8 'TNSB38R '
168900931105     O                       RSUT                45
169000990802     O                                           82 'TRASF. TITASP -> SALVAT.'
169100931115     O                                           90 'DAL:'
169200950309     O                       PARDTI              99
169300931115     O                                          110 'AL:'
169400950309     O                       PARDTF             119
169500020220     O                                          128 '/PT'
169600020220     O                       PARDFP             132
169700931105     O                                          150 'Data:'
169800950717     O                       DSDAT6             159 '  /  /  '
169900931105     O                                          167 'Ora:'
170000950717     O                       DTORA6             178 '  .  .  '
170100931105     O                                          190 'Pagina:'
170200931105     O                       PAGE          Z    198
170300931105     O          E            TESTA       1  1
170400931105     O                       TES(2)              66
170500931105     O                       TES(3)             132
170600931105     O                       TES(4)             198
170700931105     O          E            TESTA          1
170800931105     O                       TES(1)              66
170900931105     O                       TES(1)             132
171000931105     O                       TES(1)             198
171100001019     O*------------------------------------
171200001019     O          E            ERRDCM         1
171300001019     O                       TASAAS            +  5
171400001019     O                       TASLNP            +  1
171500001019     O                       TASNRS            +  1
171600001019     O                       TASNSP            +  1
171700001019     O                       TASTBL            +  1
171800001019     O                                         +  1 'BOLLA NON CONS.    '
171900931105     O*------------------------------------
172000950307     O          E            ERRTAS         1
172100950307     O                       TASAAS            +  5
172200950307     O                       TASLNP            +  1
172300950307     O                       TASNRS            +  1
172400950307     O                       TASNSP            +  1
172500950307     O                       TASTBL            +  1
172600990802     O                                         +  1 'ERR. WRITE TITAS   '
172700950307     O*------------------------------------
172800950307     O          E            ERRTA7         1
172900950307     O                       TA7AAS            +  5
173000950307     O                       TA7LNP            +  1
173100950307     O                       TA7NRS            +  1
173200950307     O                       TA7NSP            +  1
173300950307     O                       TA7TBL            +  1
173400950307     O                       TA7SVN            +  1
173500990802     O                                         +  1 'ERR. WRITE TITA7   '
173600990802     O*------------------------------------
173700990802     O          E            ERRTAA         1
173800990802     O                       TAAAAS            +  5
173900990802     O                       TAALNP            +  1
174000990802     O                       TAANRS            +  1
174100990802     O                       TAANSP            +  1
174200990802     O                       TAATRC            +  1
174300990802     O                                         +  1 'ERR. WRITE TITAA   '
174400990802     O*------------------------------------
174500990802     O          E            ERRTAT         1
174600990802     O                       TATAAS            +  5
174700990802     O                       TATLNP            +  1
174800990802     O                       TATNRS            +  1
174900990802     O                       TATNSP            +  1
175000990802     O                       TATNSC            +  1
175100990802     O                                         +  1 'ERR. WRITE TITAT   '
175200990802     O*------------------------------------
175300990802     O          E            ERRTA4         1
175400990802     O                       TA4AAS            +  5
175500990802     O                       TA4LNP            +  1
175600990802     O                       TA4NRS            +  1
175700990802     O                       TA4NSP            +  1
175800990802     O                       TA4TRC            +  1
175900990802     O                                         +  1 'ERR. WRITE TITA4   '
176000010903     O*------------------------------------
176100010903     O          E            ERRTAH         1
176200010903     O                       TAHAAS            +  5
176300010903     O                       TAHLNP            +  1
176400010903     O                       TAHNRS            +  1
176500010903     O                       TAHNSP            +  1
176600010903     O                       TAHTRC            +  1
176700010903     O                                         +  1 'ERR. WRITE TITAH   '
176800950307     O*------------------------------------
176900950307     O          E            ERRCET         1
177000950307     O                       CETAAS            +  5
177100950307     O                       CETLNP            +  1
177200950307     O                       CETNRS            +  1
177300950307     O                       CETNSP            +  1
177400950307     O                       CETTBL            +  1
177500950309     O                                         +  1 'ERR. WRITE ECCET   '
177600950307     O*------------------------------------
177700950307     O          E            ERRCED         1
177800950307     O                       CEDAAS            +  5
177900950307     O                       CEDLNP            +  1
178000950307     O                       CEDNRS            +  1
178100950307     O                       CEDNSP            +  1
178200950307     O                       CEDTBL            +  1
178300950307     O                       CEDCMP            +  1
178400950309     O                                         +  1 'ERR. WRITE ECCED   '
178500980325     O*------------------------------------
178600980325     O          E            ERRFIM         1
178700980325     O                       FIMAAS            +  5
178800980325     O                       FIMLNP            +  1
178900980325     O                       FIMNRS            +  1
179000980325     O                       FIMNSP            +  1
179100980325     O                       FIMTBL            +  1
179200980325     O                                         +  1 'ERR. WRITE ECFIM   '
179300970903     O*------------------------------------
179400970903     O          E            ERRECT         1
179500970903     O                       ECTNBC        Z   +  5
179600970903     O                       ECTCMP        Z   +  1
179700970903     O                       ECTDDC            +  1 '    /  /  '
179800970903     O                                         +  1 'ERR. WRITE ECECT   '
179900970903     O*------------------------------------
180000970903     O          E            ERRECD         1
180100970903     O                       ECDNBC        Z   +  5
180200970903     O                       ECDCMP        Z   +  1
180300970903     O                       ECDFRA            +  1
180400970903     O                       ECDIOE            +  1
180500970903     O                       ECDKCC        Z   +  1
180600970903     O                       ECDKSC        Z   +  1
180700970903     O                       ECDRIC            +  1
180800970903     O                       ECDCCF        Z   +  1
180900970903     O                       ECDDDO            +  1 '    /  /  '
181000970903     O                                         +  1 'ERR. WRITE ECECD   '
181100000404     O*------------------------------------
181200000404     O          E            ERREVB         1
181300000404     O                       EVBAAS            +  5
181400000404     O                       EVBLNP            +  1
181500000404     O                       EVBNRS            +  1
181600000404     O                       EVBNSP            +  1
181700000404     O                                         +  1 'ERR. WRITE FNEVB   '
181800000404     O*------------------------------------
181900000404     O          E            ERRGCP         1
182000000404     O                       GCPAAS            +  5
182100000404     O                       GCPLNP            +  1
182200000404     O                       GCPNRS            +  1
182300000404     O                       GCPNSP            +  1
182400050301     O                                         +  1 'ERR. WRITE TIGCP   '
182500050303     O          E            ERRGCP1        1
182600050303     O                       GCPAAS            +  5
182700050303     O                       GCPLNP            +  1
182800050303     O                       GCPNRS            +  1
182900050303     O                       GCPNSP            +  1
183000050303     O                                         +  1 'GIACENZA ANCORA IN FILIALE'
183100000404     O*------------------------------------
183200000404     O          E            ERRGAP         1
183300000404     O                       GAPAAS            +  5
183400000404     O                       GAPLNP            +  1
183500000404     O                       GAPNRS            +  1
183600000404     O                       GAPNSP            +  1
183700050301     O                                         +  1 'ERR. WRITE TIGAP   '
183800000404     O*------------------------------------
183900000404     O          E            ERRGNP         1
184000000404     O                       GNPAGC            +  5
184100000404     O                       GNPFGC            +  4
184200000404     O                       GNPNGC            +  1
184300050301     O                                         +  1 'ERR. WRITE TIGNP   '
184400040303     O*------------------------------------
184500040303     O          E            ERRAR5         1
184600040303     O                       AR5AAS            +  5
184700040303     O                       AR5LNP            +  1
184800040303     O                       AR5NRS            +  1
184900040303     O                       AR5NSP            +  1
185000040303     O                                         +  1 'ERR. WRITE FIAR5   '
185100060919     O          E            ERRtai         1
185200060919     O                       taiAAS            +  5
185300060919     O                       taiLNP            +  1
185400060919     O                       taiNRS            +  1
185500060919     O                       taiNSP            +  1
185600060919     O                       taitbl            +  1
185700060919     O                                         +  1 'ERR. WRITE TITAI   '
185800081103     O          E            ERRrab         1
185900081103     O                       rabAAS            +  5
186000081103     O                       rabLNP            +  1
186100081103     O                       rabNRS            +  1
186200081103     O                       rabNSP            +  1
186300081103     O                                         +  1 'ERR. WRITE FIRAB   '
186400950309     O*------------------------------------
186500000404     O          E            NOCCSB         1
186600950309     O                       CSBAAS            +  5
186700950309     O                       CSBLNP            +  1
186800950309     O                       CSBNRS            +  1
186900950309     O                       CSBNSP            +  1
187000000404     O                                         +  1 'BOLLA NON CANC:TNCSB'
187100000404     O*------------------------------------
187200040715     O*****     E            NOCDCT         1
187300040715     O*****                  DCTAAS            +  5
187400040715     O*****                  DCTLNP            +  1
187500040715     O*****                  DCTNRS            +  1
187600040715     O*****                  DCTNSP            +  1
187700040715     O*****                                    +  1 'BOLLA NON CANC:FNDCT'
187800040715     O          E            NOCDCT         1
187900040715     O                       i12AAS            +  5
188000040715     O                       i12LNP            +  1
188100040715     O                       i12NRS            +  1
188200040715     O                       i12NSP            +  1
188300040715     O                                         +  1 'BOLLA NON CANC:FNDCT'
188400000404     O*------------------------------------
188500000404     O          E            NOCGCP         1
188600000404     O                       GCPAAS            +  5
188700000404     O                       GCPLNP            +  1
188800000404     O                       GCPNRS            +  1
188900000404     O                       GCPNSP            +  1
189000050301     O                                         +  1 'BOLLA NON CANC:TIGCP'
189100040922     O*------------------------------------
189200040922     O          E            ERRblp         1
189300040922     O                       blpAAS            +  5
189400040922     O                       blpLNP            +  1
189500040922     O                       blpNRS            +  1
189600040922     O                       blpNSP            +  1
189700040922     O                                         +  1 'ERR. Trovato reK su FNBLP'
189800040922     O*------------------------------------
189900040922     O          E            ERRarb         1
190000040922     O                       arbAAS            +  5
190100040922     O                       arbLNP            +  1
190200040922     O                       arbNRS            +  1
190300040922     O                       arbNSP            +  1
190400040922     O                                         +  1 'ERR. Trovato reK su FNARB'
190500061122     O          E            ERRSTO         1
190600061122     O                       TASAAS            +  5
190700061122     O                       TASLNP            +  1
190800061122     O                       TASNRS            +  1
190900061122     O                       TASNSP            +  1
191000061122     O                       TASTBL            +  1
191100061122     O                                         +  1 'ERR. WRITE STORICO '
191200040922     O*------------------------------------
191300950308     O          E            STAFIN         1
191400990802     O                                           30 'N.REC.TRASF. TITAS'
191500950308     O                       NUMTAS        2     44
191600950308     O          E            STAFIN         1
191700990802     O                                           30 'N.REC.TRASF. TITA7'
191800950308     O                       NUMTA7        2     44
191900990802     O          E            STAFIN         1
192000990802     O                                           30 'N.REC.TRASF. TITAA'
192100990802     O                       NUMTAA        2     44
192200990802     O          E            STAFIN         1
192300990802     O                                           30 'N.REC.TRASF. TITAT'
192400990802     O                       NUMTAT        2     44
192500990802     O          E            STAFIN         1
192600990802     O                                           30 'N.REC.TRASF. TITA4'
192700990802     O                       NUMTA4        2     44
192800010903     O          E            STAFIN         1
192900010903     O                                           30 'N.REC.TRASF. TITAH'
193000010903     O                       NUMTAH        2     44
193100950308     O          E            STAFIN         1
193200950308     O                                           30 'N.REC.TRASF. ECCET'
193300950308     O                       NUMCET        2     44
193400950308     O          E            STAFIN         1
193500950308     O                                           30 'N.REC.TRASF. ECCED'
193600950308     O                       NUMCED        2     44
193700980325     O          E            STAFIN         1
193800980325     O                                           30 'N.REC.TRASF. ECFIM'
193900980325     O                       NUMFIM        2     44
194000970903     O          E            STAFIN         1
194100970903     O                                           30 'N.REC.TRASF. ECECT'
194200970903     O                       NUMECT        2     44
194300970903     O          E            STAFIN         1
194400970903     O                                           30 'N.REC.TRASF. ECECD'
194500970903     O                       NUMECD        2     44
194600000327     O          E            STAFIN         1
194700000327     O                                           30 'N.REC.TRASF. FNEVB'
194800000327     O                       NUMEVB        2     44
194900000404     O          E            STAFIN         1
195000050301     O                                           30 'N.REC.TRASF. TIGCP'
195100000404     O                       NUMGCP        2     44
195200000404     O          E            STAFIN         1
195300050301     O                                           30 'N.REC.TRASF. TIGAP'
195400000404     O                       NUMGAP        2     44
195500000404     O          E            STAFIN         1
195600050301     O                                           30 'N.REC.TRASF. TIGNP'
195700000404     O                       NUMGNP        2     44
195800040310     O          E            STAFIN         1
195900040310     O                                           30 'N.REC. BOLLE FIAR5'
196000040310     O                       NUMAR5        2     44
196100060919     O          E            STAFIN         1
196200060919     O                                           30 'N.REC. BOLLE TITAI'
196300060919     O                       NUMTAI        2     44
196400081103     O          E            STAFIN         1
196500081103     O                                           30 'N.REC. BOLLE FIRAB'
196600081103     O                       NUMRAB        2     44
196700061122     O          E            STAFIN         1
196800061122     O                                           30 'N.BOLLE IN STORICO'
196900061122     O                       NUMSTO        2     44
197000001019     O          E            STAFIN         1
197100001020     O                                           30 'N.REC. BOLLE SCART'
197200001019     O                       NUMNON        2     44
197300021021     O          E            STAFIN         1
197400021021     O                                           30 'DI CUI:'
197500021021     O          E            STAFIN         1
197600021021     O                                           30 'X GIACENZA APERTA '
197700021021     O                       CNTGCP        2     44
197800021021     O          E            STAFIN         1
197900021021     O                                           30 'X DANNO PENDENTE  '
198000021021     O                       CNTDCT        2     44
198100021021     O          E            STAFIN         1
198200021021     O                                           30 'X C/A ATTIVO      '
198300021021     O                       CNTCSB        2     44
198400040922     O          E            STAFIN         1
198500040922     O                                           30 'N.REK.Trovati FNBLP'
198600040922     O                       NUMblp        2     44
198700040922     O          E            STAFIN         1
198800040922     O                                           30 'N.REK.Trovati FNARB'
198900040922     O                       NUMarb        2     44
199000060626     O          E            STAFIN         1
199100060626     O                                           +0 'Chiusuara pgm: FINTAS'
199200060626     O                       fintas              +1
199300060626     O                       tasdsp              +1
199400060626     O                       dsdatf              +1
199500060626     O               98                          +1 'SHTDN'
199600060626     O                                           +1 'FINL00'
199700060626     O                       finl00              +1
199800950308     O          E            STAFIN           60
199900931115     O                                           76 '*** FINE STAMPA ***'
200000931115     O                                          150 'Data:'
200100950717     O                       DSDAT6             159 '  /  /  '
200200990802     O                                          167 'Ora:'
200300950717     O                       DTORA6             178 '  :  :  '
200400940912     O*------------------------------------------------------------------------*
200500970903     O* AGGIORNAMENTO FILES
200600970903     O*------------------------------------------------------------------------*
200700970903     OECECT     E            AGGECT
200800970903     O                       ECTATB
200900970903     O                       ECTFTR
201000970903     OECECD     E            AGGECD
201100970903     O                       ECDATB
201200970903     O                       ECDFTR
201300970903     O*------------------------------------------------------------------------*
201400040823** cmd
201500040823OVRDBF FILE(        ) TOFILE(
201600931105**  TES - RIGHE IN STAMPA
201700931105------------------------------------------------------------------  1
201800931105 DATA    ORA       AA LNP SE N.SPED. TB     ERR  DESCRIZIONE ERROR  2
201900950308E                                                                   3
202000931105                                                                    4
202100931105                                                                    5
202200931105                                                                    6
202300931105                                                                    7
202400931105                                                                    8
202500931105                                                                    9
202600931105                                                                   10
202700931105                                                                   11
202800931105                                                                   12
202900931105                                                                   13
203000931105                                                                   14
203100931105                                                                   15
