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
014600131223      *------------------------------------------------------------------------*
014700131223
014800131223     D*------------------
014900131223     D* VARIABILI D WRK
015000131223     D*------------------
015100131223     D  wAAS           S                   like(tasAAS) inz
015200131223     D  wLNP           S                   like(tasLNP) inz
015300131223     D  wNRS           S                   like(tasNRS) inz
015400131223     D  wNSP           S                   like(tasNSP) inz
015500131223
015600131223     D*------------------
015700131223     D* LINKING A DEFINIZIONI ESTERNE
015800131223     D*------------------
015900131223
016000131223     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
016100131223     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
016200131223
016300940912     D*------------------------------------------------------------------------*
016400940912     D* SCHIERE
016500940912     D*------------------------------------------------------------------------*
016600040823     d cmd             s             60    dim(01) ctdata perrcd(1)
016700931105     D TES             S             66    DIM(15) CTDATA PERRCD(1)
016800080925     d tbl             s              2    dim(50) inz
016900950309     D* DS TABELLA $3 (DATE AVANZAMENTO TRASFERIMENTO BOLLE)
017000950309     D DS$3          E DS
017100080925     D DStb          E DS
017200950717     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
017300950717     D DS§DAT          DS
017400950717     D  DSSEC                  1      2  0
017500950717     D  DSANN                  3      4  0
017600950717     D  DSMES                  5      6  0
017700950717     D  DSGIO                  7      8  0
017800950717     D  DSAAS                  1      4  0
017900950717     D  DSMGS                  5      8  0
018000950717     D  DSDAT6                 3      8  0
018100950717     D  DSDAT8                 1      8  0
018200950717     D DS§TIM          DS
018300950717     D  DTORA                  1      2  0
018400950717     D  DTMIN                  3      4  0
018500950717     D  DTSCN                  5      6  0
018600950717     D  DTGIO                  7      8  0
018700950717     D  DTMES                  9     10  0
018800950717     D  DTSEC                 11     12  0
018900950717     D  DTANN                 13     14  0
019000950717     D  DTORA6                 1      6  0
019100950717     D  DTTIME                 1     14  0
019200940912     D*------------------------------------------------------------------------*
019300940912     D* INPUT
019400940912     D*------------------------------------------------------------------------*
019500941123     D                 DS
019600941123     D  DSAASI                 1      4  0
019700941123     D  DSMGSI                 5      8  0
019800950309     D  DSDATI                 1      8  0
019900941123     D                 DS
020000941123     D  DSAASF                 1      4  0
020100941123     D  DSMGSF                 5      8  0
020200950309     D  DSDATF                 1      8  0
020300020220     D                 DS
020400020220     D  DSAASFP                1      4  0
020500020220     D  DSMGSFP                5      8  0
020600020220     D  DSDATFP                1      8  0
020700960413     D*-------------------
020800960413     D* DATA SPEDIZIONE
020900960413     D*-------------------
021000960413     D                 DS
021100960413     D  TASAAS                 1      4  0
021200960413     D  TASMGS                 5      8  0
021300960413     D  TASDSP                 1      8  0
021400000327     D*------------------
021500000327     D* DS "XSRDA8" - CONTROLLA DATA (8)
021600000327     D*------------------
021700000327     D WLBDA8          DS                  INZ
021800000327     D  G08DAT                 1      8  0
021900000327     D  G08INV                 9     16  0
022000000327     D  G08ERR                17     17
022100000327     D  G08TGI                18     22  0
022200040715
022300040715     d Fidn12ds      e ds
022400040715     d  skKey                 26    305    dim(20)
022500040715     d  skAnn                306    325    dim(20)
022600040715     d  skDca                326    485    dim(20)
022700040715     d  skFca                486    545  0 dim(20)
022800040715     d  skDch                546    705  0 dim(20)
022900040715     d  skCch                706    745    dim(20)
023000040715
023100940912     D*-------------------
023200931115     D* PARAMETRI LANCIO PGM
023300940912     D*-------------------
023400000327     D PARAM           DS
023500950309     D  PARDTI                 1      8  0                                      DT.INI
023600950309     D  PARDTF                 9     16  0                                      DT.FIN
023700020220     D  PARDFP                17     24  0                                      DT.FIN PT
023800050706     D  PARcop                25     27                                         DT.FIN PT
023900940912     D*-------------------
024000940912     D* ARCHITETTURA
024100940912     D*-------------------
024200931115     D KPJBA         E DS
024300050705     D tnsb34ds      E DS
024400050705     D dtasflo       E DS
024500061121      *
024600061121     D*-------------------
024700061121     D* ANAGRAFICA
024800061121     D*-------------------
024900061121     d TIBS69ds      e ds                  inz
025000061121     d DS_cnaco      e ds                  inz  extname(CNACO00F)
025100061121     d DS_cnind      e ds                  inz  extname(CNIND00F)
025200061121     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
025300061121     d DS_fncls      e ds                  inz  extname(FNCLS00F)
025400061122     D*-------------------
025500061122     D* RIFERIMENTI ALFANUMERICI
025600061122     D*-------------------
025700061122     d DTA4A         e ds
025800061122     D*-------------------
025900061122     D* CODICE FISCALE
026000061122     D*-------------------
026100061122     d DTA4Q         e ds
026200120709
026300120709      * RCD DOC FIAR5
026400120709     d dAR5doc       e ds
026500120709
026600940912     C*------------------------------------------------------------------------*
026700940912     C*?MAIN LINE
026800940912     C*------------------------------------------------------------------------*
026900000000     C*
027000930129     C* ROUTINE INIZIALE
027100930129     C                   EXSR      RUTINZ
027200931105     C*
027300931105     C* STAMPA RIGA DI TESTATA
027400931105     C                   EXCEPT    TESTA
027500930601     C*
027600930601     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
027700930601     C     FINL00        IFEQ      '0'                                          --- 1 -->
027800000000     C*
027900990802     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA SU TITAS
028000950307     C                   EXSR      SETTAS
028100930128     C*
028200990802     C* CICLO FINO A FINE FILE TITAS OPPURE
028300950308     C* RICHIESTA CHIUSURA PGM OPPURE
028400950308     C* FINE PGM PER ERRORE
028500950308     C                   SETOFF                                       98
028600950307     C     FINTAS        DOWEQ     '0'                                          --- 2 -->
028700950308     C     *IN98         ANDEQ     '0'
028800950308     C     FINL00        ANDEQ     '0'
028900080214      * verifico   che non esistano ARB e BLP altrimenti non cancello
029000040823      * e leggo un'altro rekord
029100040823     C     Kbol          Chain     fnblp01l                           33
029200040922     C  N33              ADD       1             NUMblp                         *REC TRASF TITA7
029300040922     C  n33              EXCEPT    ERRblp
029400080214     C     kbol          Chain     fnarb01l                           34
029500080214     C  N34              ADD       1             NUMarb                         *REC TRASF TITA7
029600080214     C  n34              EXCEPT    ERRarb
029700080214     c                   if        not *in33 or not *in34
029800080214     c                   goto      altrorek
029900080214     c                   end
030000950307     C*
030100001019     C                   WRITE     TITASS00                             99      *SCRIVO TITASS
030200001019     C  N99              DELETE    TITASP00                             99      *CANCELLO TITASP
030300950307     C   99              EXCEPT    ERRTAS
030400950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
030500990802     C  N99              ADD       1             NUMTAS                         *REC TRASF TITAS
030600050705     c* se bolla ha immagine di laguna scrivo record sul file TILAD00F
030700050705     c                   if        not *in99
030800050705     c                   movel     tasflo        dtasflo
030900050705     c                   if        §floiml <> ' '
031000050705     c                   exsr      srlaguna
031100050705     c                   end
031200120709       //?se la bolla ha le immagini dei documenti firmati dal cliente da
031300120709       //?tornare al mittente scrivo nuovo record sul file TIJDD00F
031400120709      /free
031500120709        AR5trd = 'DOC';
031600120709        chain (TASaas:TASlnp:TASnrs:TASnsp:AR5trd) FIAR531C;
031700120709        IF  %found(FIAR531C);
031800120709          dAR5doc = AR5uni;
031900120709          IF  §AR5jfid <> *blanks;
032000120709            exsr srJdoc;
032100120709          ENDIF;
032200120709        ENDIF;
032300120709      /end-free
032400050705     c                   end
032500950307     C*
032600990802     C* SE ESISTE TITA7 TRASFERISCO ANCHE QUELLO
032700950307     C     TASSV3        IFNE      *BLANKS                                      --- 3 -->
032800990802     C                   EXSR      SETTA7                                       *PRIMO TITA7
032900950307     C     FINTA7        DOWEQ     '0'                                          --- 4 -->
033000990802     C                   WRITE     TITA7S00                             99      *SCRIVO TITA7S
033100001019     C  N99              DELETE    TITA730C                             99      *CANCELLO TITA730C
033200950307     C   99              EXCEPT    ERRTA7
033300950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
033400990802     C  N99              ADD       1             NUMTA7                         *REC TRASF TITA7
033500990802     C                   EXSR      LETTA7                                       *SUCCESSIVO TITA7
033600950307     C                   ENDDO                                                  <-- 4 ---
033700950307     C                   ENDIF                                                  <-- 3 ---
033800990802     C*
033900990802     C* TRASFERISCE TITAA - ANAGRAFICHE
034000990802     C                   EXSR      SETTAA                                       *PRIMO TITAA
034100990802DO  3C     FINTAA        DOWEQ     '0'
034200990802     C                   WRITE     TITAAS00                             99      *SCRIVO TITAAS
034300001019     C  N99              DELETE    TITAA30C                             99      *CANCELLO TITAA30C
034400990802     C   99              EXCEPT    ERRTAA
034500990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
034600990802     C  N99              ADD       1             NUMTAA                         *REC TRASF TITAA
034700990802     C                   EXSR      LETTAA                                       *SUCCESSIVO TITAA
034800990802E   3C                   ENDDO
034900061121     C* verifico se scritto i dati anagrafici del mittente altrimenti aggancio anagrafica
035000061121     c                   if        tasrsm = *blanks
035100061121     c                   exsr      recana
035200061121     c                   endif
035300990802     C*
035400990802     C* TRASFERISCE TITAT - SEGNACOLLI
035500990802     C                   EXSR      SETTAT                                       *PRIMO TITAT
035600990802DO  3C     FINTAT        DOWEQ     '0'
035700990802     C                   WRITE     TITATS00                             99      *SCRIVO TITATS
035800001019     C  N99              DELETE    TITAT30C                             99      *CANCELLO TITAT30C
035900990802     C   99              EXCEPT    ERRTAT
036000990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
036100990802     C  N99              ADD       1             NUMTAT                         *REC TRASF TITAT
036200990802     C                   EXSR      LETTAT                                       *SUCCESSIVO TITAT
036300990802E   3C                   ENDDO
036400990802     C*
036500990802     C* TRASFERISCE TITA4 - RIFERIMENTI
036600990802     C                   EXSR      SETTA4                                       *PRIMO TITA4
036700990802DO  3C     FINTA4        DOWEQ     '0'
036800990802     C                   WRITE     TITA4S00                             99      *SCRIVO TITA4S
036900001019     C  N99              DELETE    TITA430C                             99      *CANCELLO TITA430C
037000990802     C   99              EXCEPT    ERRTA4
037100990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
037200990802     C  N99              ADD       1             NUMTA4                         *REC TRASF TITA4
037300990802     C                   EXSR      LETTA4                                       *SUCCESSIVO TITA4
037400990802E   3C                   ENDDO
037500010903     C*
037600010903     C* TRASFERISCE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
037700010903     C                   EXSR      SETTAH                                       *PRIMO TITAH
037800010903DO  3C     FINTAH        DOWEQ     '0'
037900010903     C                   WRITE     TITAHS00                             99      *SCRIVO TITAHS
038000010903     C  N99              DELETE    TITAH30C                             99      *CANCELLO TITAH30C
038100010903     C   99              EXCEPT    ERRTAH
038200010903     C   99              MOVEL     '1'           FINL00                         *FINE PGM
038300010903     C  N99              ADD       1             NUMTAH                         *REC TRASF TITAH
038400010903     C                   EXSR      LETTAH                                       *SUCCESSIVO TITAH
038500010903E   3C                   ENDDO
038600950307     C*
038700950307     C* SE ESISTE ECCET LO TRASFERISCE
038800971008     C                   Z-ADD     999           MINNBC                         MIMIMO BLOCCO
038900950307     C                   Z-ADD     TASAAS        CETAAS                         ANNO SPEDIZIONE
039000950307     C                   Z-ADD     TASLNP        CETLNP                         LINEA DI PARTENZA
039100950307     C                   Z-ADD     TASNRS        CETNRS                         NUMERO SERIE
039200950307     C                   Z-ADD     TASNSP        CETNSP                         NUMERO SPEDIZIONE
039300950307     C                   MOVEL     TASTBL        CETTBL                         TIPO BOLLA
039400950307     C     KEYCET        CHAIN     ECCET30C                           99
039500950307     C     *IN99         IFEQ      *OFF                                         --- 3 --> *TROVATO
039600970903     C*
039700970903     C* MEMORIZZA IL NUMERO BLOCCO COMPETENZE TESTATA PIU' PICCOLO
039800971008IF  4C     CETNBC        IFGT      *ZEROS
039900971008IF  5C     CETNBC        IFLT      MINNBC
040000970903     C                   Z-ADD     CETNBC        MINNBC
040100971008E   5C                   ENDIF
040200971008E   4C                   ENDIF
040300970903     C*
040400950309     C                   WRITE     ECCETS                               99      *SCRIVO ECCETS
040500001019     C  N99              DELETE    ECCET30C                             99      *CANCELLO ECCET30C
040600950307     C   99              EXCEPT    ERRCET
040700950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
040800950308     C  N99              ADD       1             NUMCET                         *REC TRASF ECCET
040900950307     C*
041000950307     C* SE ESISTE ECCED TRASFERISCO ANCHE QUELLO
041100950307     C     CETDET        IFEQ      'S'                                          --- 4 -->
041200950307     C                   EXSR      SETCED                                       *PRIMO ECCED
041300950307     C     FINCED        DOWEQ     '0'                                          --- 5 -->
041400950309     C                   WRITE     ECCEDS                               99      *SCRIVO ECCEDS
041500001020     C  N99              DELETE    ECCED30C                             99      *CANCELLO ECCED30C
041600950307     C   99              EXCEPT    ERRCED
041700950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
041800950308     C  N99              ADD       1             NUMCED                         *REC TRASF ECCED
041900950307     C                   EXSR      LETCED                                       *SUCCESSIVO ECCED
042000950307     C                   ENDDO                                                  <-- 5 ---
042100950307     C                   ENDIF                                                  <-- 4 ---
042200950307     C                   ENDIF                                                  <-- 3 ---
042300980325     C*
042400980325     C* SE ESISTE ECFIM LO TRASFERISCE
042500980325     C                   Z-ADD     TASAAS        FIMAAS                         ANNO SPEDIZIONE
042600980325     C                   Z-ADD     TASLNP        FIMLNP                         LINEA DI PARTENZA
042700980325     C                   Z-ADD     TASNRS        FIMNRS                         NUMERO SERIE
042800980325     C                   Z-ADD     TASNSP        FIMNSP                         NUMERO SPEDIZIONE
042900980325     C                   MOVEL     TASTBL        FIMTBL                         TIPO BOLLA
043000001019     C     KEYFIM        CHAIN     ECFIM30C                           99
043100980325     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
043200980325     C                   WRITE     ECFIMS00                             99      *SCRIVE
043300001019     C  N99              DELETE    ECFIM30C                             99      *CANCELLA
043400980325     C   99              EXCEPT    ERRFIM
043500980325     C   99              MOVEL     '1'           FINL00                         *FINE PGM
043600980325     C  N99              ADD       1             NUMFIM                         *REC TRASF ECCET
043700011127     C     KEYFIM        READE     ECFIM30C                               99
043800980325     C                   ENDDO                                                  <-- 3 ---
043900000327     C*
044000000327     C* SE ESISTE FNEVB LO TRASFERISCE
044100000327     C                   Z-ADD     TASAAS        EVBAAS                         ANNO SPEDIZIONE
044200000327     C                   Z-ADD     TASLNP        EVBLNP                         LINEA DI PARTENZA
044300000327     C                   Z-ADD     TASNRS        EVBNRS                         NUMERO SERIE
044400000327     C                   Z-ADD     TASNSP        EVBNSP                         NUMERO SPEDIZIONE
044500001019     C     KEYEVB        CHAIN     FNEVB01L                           99
044600000327     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
044700000327     C                   WRITE     FNEVBS                               99      *SCRIVE
044800001019     C  N99              DELETE    FNEVB01L                             99      *CANCELLA
044900131029     c                   if        not *in99
045000131029     C     Kevb2f        chain     FNEVB22L
045100131029     c                   if        %found(fnevb22l)
045200131029     c                   delete    fnevb22l
045300131029     c                   endif
045400131029     c                   endif
045500000327     C   99              EXCEPT    ERREVB
045600000327     C   99              MOVEL     '1'           FINL00                         *FINE PGM
045700000327     C  N99              ADD       1             NUMEVB                         *REC TRASF FNEVB
045800000327     C     KEYEVB        READE     FNEVB01L                               99
045900000327     C                   ENDDO                                                  <-- 3 ---
046000000404     C*
046100050301     C* SE ESISTE TIGCP LO TRASFERISCE
046200001018     C     TASFGC        IFEQ      'S'
046300000404     C                   Z-ADD     TASAAS        KCPAAS                         ANNO SPEDIZIONE
046400000404     C                   Z-ADD     TASLNP        KCPLNP                         LINEA DI PARTENZA
046500000404     C                   Z-ADD     TASNRS        KCPNRS                         NUMERO SERIE
046600000404     C                   Z-ADD     TASNSP        KCPNSP                         NUMERO SPEDIZIONE
046700050301     C     KE2GCP        CHAIN     TIGCP51L                           99
046800001018     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
046900050303     c     gcpsede       ifeq      'S'
047000050301     C                   WRITE     TIGCPS                               99      *SCRIVE
047100050301     C  N99              DELETE    TIGCP51L                             99      *CANCELLA
047200000404     C   99              EXCEPT    ERRGCP
047300000404     C   99              MOVEL     '1'           FINL00                         *FINE PGM
047400000404     C  N99              ADD       1             NUMGCP                         *REC TRASF FNEVB
047500000404     C*
047600050301     C* SE ESISTE TIGAP LO TRASFERISCE
047700000404     C                   Z-ADD     GCPAAS        KAPAAS                         ANNO SPEDIZIONE
047800000404     C                   Z-ADD     GCPLNP        KAPLNP                         LINEA DI PARTENZA
047900000404     C                   Z-ADD     GCPNRS        KAPNRS                         NUMERO SERIE
048000000404     C                   Z-ADD     GCPNSP        KAPNSP                         NUMERO SPEDIZIONE
048100050301     C     KEYGAP        CHAIN     TIGAP01L                           99
048200001018     C     *IN99         DOWEQ     *OFF
048300050301     C                   WRITE     TIGAPS                               99      *SCRIVE
048400050301     C  N99              DELETE    TIGAP01L                             99      *CANCELLA
048500001017     C   99              EXCEPT    ERRGAP
048600001017     C   99              MOVEL     '1'           FINL00                         *FINE PGM
048700001017     C  N99              ADD       1             NUMGAP                         *REC TRASF FNEVB
048800050301     C     KEYGAP        READE     TIGAP01L                               99
048900001018     C                   ENDDO
049000000404     C*
049100050301     C* SE ESISTE TIGNP LO TRASFERISCE
049200000404     C                   Z-ADD     GCPAGC        KNPAGC                         ANNO APERTURA
049300000404     C                   Z-ADD     GCPFGC        KNPFGC                         FILIALE APERTURA
049400000404     C                   Z-ADD     GCPNGC        KNPNGC                         NUMERO GIACENZA
049500050301     C     KEYGNP        CHAIN     TIGNP01L                           99
049600001018     C     *IN99         DOWEQ     *OFF
049700050301     C                   WRITE     TIGNPS                               99      *SCRIVE
049800050301     C  N99              DELETE    TIGNP01L                             99      *CANCELLA
049900001017     C   99              EXCEPT    ERRGNP
050000001017     C   99              MOVEL     '1'           FINL00                         *FINE PGM
050100001017     C  N99              ADD       1             NUMGNP                         *REC TRASF FNEVB
050200050301     C     KEYGNP        READE     TIGNP01L                               99
050300001018     C                   ENDDO
050400050303     c                   else
050500050303     C                   EXCEPT    ERRGCP1
050600060707     C*                  MOVEL     '1'           FINL00                         *FINE PGM
050700050303     C                   ENDIF
050800000404     C*
050900000404     C* LETTURA SUCCESSIVA GIACENZE
051000050301     C     KE2GCP        READE     TIGCP51L                               99
051100001018     C                   ENDDO
051200001018     C                   ENDIF
051300040303     C*
051400040303     C* SE ESISTE FIAR5 LO TRASFERISCE
051500040303     C                   Z-ADD     TASAAS        AR5AAS                         ANNO SPEDIZIONE
051600040303     C                   Z-ADD     TASLNP        AR5LNP                         LINEA DI PARTENZA
051700040303     C                   Z-ADD     TASNRS        AR5NRS                         NUMERO SERIE
051800040303     C                   Z-ADD     TASNSP        AR5NSP                         NUMERO SPEDIZIONE
051900040531     C     KEYAR5        CHAIN     FIAR531C                           99
052000040303     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
052100040303     C                   WRITE     FIAR5S                               99      *SCRIVE
052200040531     C  N99              DELETE    FIAR531C                             99      *CANCELLA
052300040303     C   99              EXCEPT    ERRAR5
052400040303     C   99              MOVEL     '1'           FINL00                         *FINE PGM
052500040303     C  N99              ADD       1             NUMAR5                         *REC TRASF FNEVB
052600040531     C     KEYAR5        READE     FIAR531C                               99
052700040303     C                   ENDDO                                                  <-- 3 ---
052800060919     C*
052900060919     C* SE ESISTE TITAI LO TRASFERISCE
053000060919     C                   Z-ADD     TASAAS        taiAAS                         ANNO SPEDIZIONE
053100060919     C                   Z-ADD     TASLNP        taiLNP                         LINEA DI PARTENZA
053200060919     C                   Z-ADD     TASNRS        taiNRS                         NUMERO SERIE
053300060919     C                   Z-ADD     TASNSP        taiNSP                         NUMERO SPEDIZIONE
053400060919     C                   movel     TASTBL        taiTBL                         NUMERO SPEDIZIONE
053500060919     C     KEYtai        CHAIN     titai01l                           99
053600060919     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
053700060919     C                   WRITE     titaiS                               99      *SCRIVE
053800060919     C  N99              DELETE    titai01l                             99      *CANCELLA
053900060919     C   99              EXCEPT    ERRtai
054000060919     C   99              MOVEL     '1'           FINL00                         *FINE PGM
054100060919     C  N99              ADD       1             NUMtai                         *REC TRASF FNEVB
054200060919     C     KEYtai        READE     titai01l                               99
054300060919     C                   ENDDO                                                  <-- 3 ---
054400081103     C* SE ESISTE FIRAB LO TRASFERISCE
054500081103     C                   Z-ADD     TASAAS        rabAAS                         ANNO SPEDIZIONE
054600081103     C                   Z-ADD     TASLNP        rabLNP                         LINEA DI PARTENZA
054700081103     C                   Z-ADD     TASNRS        rabNRS                         NUMERO SERIE
054800081103     C                   Z-ADD     TASNSP        rabNSP                         NUMERO SPEDIZIONE
054900081103     C     KEYrab        CHAIN     firab01l                           99
055000081103     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
055100081103     C                   WRITE     firabS                               99      *SCRIVE
055200081103     C  N99              DELETE    firab01l                             99      *CANCELLA
055300081103     C   99              EXCEPT    ERRrab
055400081103     C   99              MOVEL     '1'           FINL00                         *FINE PGM
055500081103     C  N99              ADD       1             NUMrab                         *REC TRASF FNEVB
055600081103     C     KEYrab        READE     firab01l                               99
055700081103     C                   ENDDO                                                  <-- 3 ---
055800061214      * decodifico alcuni dati per il nuovo storico  e lo scrivo se non bolla Poste
055900061214     c                   If        tastsp <> 'P'
056000080926     c* se bolla di recupero non scrivo storico
056100080926     c                   z-add     1             conta
056200080926     c     tastbl        lookup    tbl(conta)                             44
056300080926     c                   if        not *in44
056400061122     c                   EXSR      wrttassto
056500061214     c                   endif
056600080926     c                   endif
056700061122
056800000000     C*
056900980325     C* LETTURA SUCCESSIVA BOLLE
057000040823     c     altrorek      tag
057100040823
057200060707     c                   if        finl00 = '0'
057300990802     C                   EXSR      LETTAS                                       *SUCCESSIVO TITAS
057400060707     c                   end
057500950308     C                   SHTDN                                        98        *CONTR.CHIUSURA PGM
057600930128     C*
057700950307     C                   ENDDO                                                  <-- 2 ---
057800950307     C                   ENDIF                                                  <-- 1 ---
057900970903     C*
058000970903     C* PULISCE I CODICI COMPETENZA -TESTATA E DETTAGLIO- CON NUMERO BLOCCO
058100970903     C* MINORE DI QUELLO PIU' PICCOLO CON CUI SONO STATE CALCOLATE LE BOLLE
058200971008IF  1C     MINNBC        IFNE      999
058300970903     C     *LOVAL        SETLL     ECECT01L
058400970903     C                   READ      ECECT01L                               97
058500971008DO  2C     *IN97         DOWEQ     *OFF
058600971008IF  3C     ECTATB        IFEQ      *BLANKS
058700971008IF  4C     ECTNBC        IFLT      MINNBC
058800970903     C                   MOVEL     'A'           ECTATB
058900970903     C                   MOVEL     *BLANKS       ECTFTR
059000970903     C                   EXCEPT    AGGECT                                       *AGGIORNAMENTO CAMPI
059100970903     C                   WRITE     ECECTS                               97      *SCRIVO ECECTS
059200970903     C   97              EXCEPT    ERRECT                                       *SCRIVE ERRORE
059300970903     C   97              MOVEL     '1'           FINL00                         *FINE PGM
059400970903     C  N97              ADD       1             NUMECT                         *REC TRASF ECECT
059500971008E   4C                   ENDIF
059600971008E   3C                   ENDIF
059700970903     C                   READ      ECECT01L                               97
059800971008E   2C                   ENDDO
059900950309     C*
060000970903     C     *LOVAL        SETLL     ECECD01L
060100970903     C                   READ      ECECD01L                               97
060200971008DO  2C     *IN97         DOWEQ     *OFF
060300971008IF  3C     ECDATB        IFEQ      *BLANKS
060400971008IF  4C     ECDNBC        IFLT      MINNBC
060500970903     C                   MOVEL     'A'           ECDATB
060600970903     C                   MOVEL     *BLANKS       ECDFTR
060700970903     C                   EXCEPT    AGGECD                                       *AGGIORNAMENTO CAMPI
060800970903     C                   WRITE     ECECDS                               97      *SCRIVO ECECDS
060900970903     C   97              EXCEPT    ERRECD                                       *SCRIVE ERRORE
061000970903     C   97              MOVEL     '1'           FINL00                         *FINE PGM
061100970903     C  N97              ADD       1             NUMECD                         *REC TRASF ECECD
061200971008E   4C                   ENDIF
061300971008E   3C                   ENDIF
061400970903     C                   READ      ECECD01L                               97
061500971008E   2C                   ENDDO
061600971008E   1C                   ENDIF
061700970903     C*
061800950309     C* AGGIORNA LA TABELLA CON L'ULTIMA DATA DI TRASFERIM. (SOLO SE TUTTO OK)
061900950309     C     *IN98         IFEQ      '0'
062000950309     C     FINL00        ANDEQ     '0'
062100950309     C                   EXSR      AGGTAB
062200950309     C                   ENDIF
062300931105     C*
062400931105     C* STAMPA RIGA DI FINE STAMPA
062500950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
062600931105     C                   EXCEPT    STAFIN
062700050705     c* chiudo il pgm di archiviazione Laguna
062800050705     C                   CLEAR                   Tnsb34ds
062900050705     C                   MOVEL     'C'           sb34tla
063000050705     C                   CALL      'TNSB34R'
063100050705     C                   PARM                    TNSB34ds
063200000000     C*
063300000000     C                   SETON                                        LR
063400940912     C*------------------------------------------------------------------------*
063500990802     C*? SETTAS - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAS
063600940912     C*------------------------------------------------------------------------*
063700950307     C     SETTAS        BEGSR
063800931115     C*
063900931115     C* AZZERO CONTATORE RECORD LETTI
064000950307     C                   MOVEL     '0'           FINTAS            1
064100930208     C*
064200930316     C* POSIZIONAMENTO E PRIMA LETTURA
064300941123     C                   Z-ADD     PARDTI        DSDATI
064400941123     C                   Z-ADD     PARDTF        DSDATF
064500020220     C                   Z-ADD     PARDFP        DSDATFP
064600941123     C                   Z-ADD     DSAASI        TASAAS
064700941123     C                   Z-ADD     DSMGSI        TASMGS
064800990802     C     KEYTAS        SETLL     TITAS38C                           99        *FINE-ARCHIV
064900930625     C*
065000930625     C     *IN99         IFEQ      '1'
065100950307     C                   MOVEL     '1'           FINTAS
065200930625     C                   ELSE
065300950307     C                   EXSR      LETTAS
065400001020     C                   ENDIF
065500930208     C*
065600930208     C                   ENDSR
065700940912     C*------------------------------------------------------------------------*
065800990802     C*? LETTAS - LETTURA PROSSIMO RECORD TITAS
065900940912     C*------------------------------------------------------------------------*
066000950307     C     LETTAS        BEGSR
066100930625     C*
066200930625     C* LEGGO FINO A:
066300930625     C*     - FINE FILE O SUPERATO LIMITE FINALE
066400930625     C*     - TROVATO RECORD VALIDO
066500930625     C                   MOVEL     'N'           WRECOK
066600950307     C     FINTAS        DOWEQ     '0'                                          --- 1 -->
066700930625     C     WRECOK        ANDEQ     'N'
066800061121      * pulisco lo storico
066900061121     c                   clear                   titassto
067000930208     C*
067100930316     C* LETTURE SUCCESSIVE ALLA PRIMA
067200990802     C                   READ      TITAS38C                               99
067300960413     C*
067400960413     C     *IN99         IFEQ      '1'                                          *FINE FILE
067500020220     C     TASDSP        ORGT      DSDATF                                       *SUPERATA DATA FINE
067600950307     C                   MOVEL     '1'           FINTAS
067700930625     C                   ELSE
067800950309     C                   EXSR      CHKTAS                                       *CONTR.VALIDITA REC.
067900001019     C                   ENDIF
068000930625     C*
068100001019     C                   ENDDO                                                  <-- 1 ---
068200930208     C*
068300930208     C                   ENDSR
068400950307     C*------------------------------------------------------------------------*
068500990802     C*? SETTA7 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA7
068600950307     C*------------------------------------------------------------------------*
068700950307     C     SETTA7        BEGSR
068800950307     C*
068900950307     C* AZZERO CONTATORE RECORD LETTI
069000950307     C                   MOVEL     '0'           FINTA7            1
069100950307     C*
069200950307     C* POSIZIONAMENTO E PRIMA LETTURA
069300950307     C                   Z-ADD     TASAAS        TA7AAS
069400950307     C                   Z-ADD     TASLNP        TA7LNP
069500950307     C                   Z-ADD     TASNRS        TA7NRS
069600950307     C                   Z-ADD     TASNSP        TA7NSP
069700950307     C                   MOVEL     TASTBL        TA7TBL
069800001019     C*
069900001019     C     KEYTA7        CHAIN     TITA730C                           99        *FINE-ARCHIV
070000950307     C     *IN99         IFEQ      '1'
070100950307     C                   MOVEL     '1'           FINTA7
070200001019     C                   ENDIF
070300950307     C*
070400950307     C                   ENDSR
070500950307     C*------------------------------------------------------------------------*
070600990802     C*? LETTA7 - LETTURA PROSSIMO RECORD TITA7
070700950307     C*------------------------------------------------------------------------*
070800001019     C     LETTA7        BEGSR
070900001020     C*
071000001020     C* LETTURE SUCCESSIVE ALLA PRIMA
071100010124     C                   READ      TITA730C                               99
071200950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
071300010124     C     TA7AAS        ORNE      TASAAS
071400010124     C     TA7LNP        ORNE      TASLNP
071500010124     C     TA7NRS        ORNE      TASNRS
071600010124     C     TA7NSP        ORNE      TASNSP
071700010124     C     TA7TBL        ORNE      TASTBL
071800950307     C                   MOVEL     '1'           FINTA7
071900001019     C                   ENDIF
072000950307     C*
072100950307     C                   ENDSR
072200990802     C*------------------------------------------------------------------------*
072300990802     C*? SETTAA - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAA - ANAGRAFICHE
072400990802     C*------------------------------------------------------------------------*
072500990802     C     SETTAA        BEGSR
072600990802     C*
072700990802     C* AZZERO CONTATORE RECORD LETTI
072800990802     C                   MOVEL     '0'           FINTAA            1
072900990802     C*
073000990802     C* POSIZIONAMENTO E PRIMA LETTURA
073100990802     C                   Z-ADD     TASAAS        TAAAAS
073200990802     C                   Z-ADD     TASLNP        TAALNP
073300990802     C                   Z-ADD     TASNRS        TAANRS
073400990802     C                   Z-ADD     TASNSP        TAANSP
073500001019     C     KEYTAA        CHAIN     TITAA30C                           99        *FINE-ARCHIV
073600990802     C     *IN99         IFEQ      '1'
073700990802     C                   MOVEL     '1'           FINTAA
073800061121      * recupero dati anagrafici del mittente
073900061121     c                   else
074000061121     c                   exsr      rectaa
074100001019     C                   ENDIF
074200990802     C*
074300990802     C                   ENDSR
074400990802     C*------------------------------------------------------------------------*
074500990802     C*? LETTAA - LETTURA PROSSIMO RECORD TITAA - ANAGRAFICHE
074600990802     C*------------------------------------------------------------------------*
074700990802     C     LETTAA        BEGSR
074800010124     C*
074900010124     C                   READ      TITAA30C                               99
075000990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
075100010130     C     TAAAAS        ORNE      TASAAS
075200010130     C     TAALNP        ORNE      TASLNP
075300010130     C     TAANRS        ORNE      TASNRS
075400010130     C     TAANSP        ORNE      TASNSP
075500990802     C                   MOVEL     '1'           FINTAA
075600001019     C                   ENDIF
075700061121      * se letto
075800061123     c                   if        fintaa = '0'
075900061121     c                   exsr      rectaa
076000061121     c                   endif
076100990802     C*
076200990802     C                   ENDSR
076300061121     C*------------------------------------------------------------------------*
076400061121     C*? RECTAA - RECUPERO DATI ANAGRAFICI MITTENTE
076500061121     C*------------------------------------------------------------------------*
076600061121     C     RECTAA        BEGSR
076700061121      * dati anagrafici mittente
076800061121     c                   IF        taatrc = 'M'
076900061121     c                   eval      tasrsm = taarsc
077000061121     c                   eval      tasinm = taaind
077100061121     c                   eval      tascam = taacap
077200061121     c                   eval      taslom = taaloc
077300061121     c                   eval      tasprm = taaprv
077400061121     c                   eval      tasnzm = taanaz
077500061122      * iva  per i franchi non coidficati
077600061122     c                   if        %subst(tastbl:1:1) = 'F'
077700061122     c                   eval      tasiva = taacpi
077800061122     c                   endif
077900061122
078000061121     c                   endif
078100061121
078200061121     c                   endsr
078300061121     C*------------------------------------------------------------------------*
078400061121     C*? RECANA - RECUPERO DA ANAGRAFICO CLIENTE
078500061121     C*------------------------------------------------------------------------*
078600061121     C     RECANA        BEGSR
078700061121     C                   clear                   ds_CNACO
078800061121     C                   clear                   ds_CNIND
078900061121     C                   clear                   ds_CNCLP
079000061121     C                   clear                   ds_FNCLS
079100061121     c                   z-add     TASccm        I69kac
079200061121     c                   z-add     TASccm        I69kin
079300061121     c                   call      'TIBS69R'
079400061121     c                   parm                    TIBS69ds
079500061121     c                   parm                    ds_CNACO
079600061121     c                   parm                    ds_CNIND
079700061121     c                   parm                    ds_CNCLP
079800061121     c                   parm                    ds_FNCLS
079900061122if  3c                   if        O69err <> *on
080000061122     c                   movel     ACOrag        TASrsm
080100061122     c                   movel     INDvia        TASinm
080200061122     c                   movel     INDcap        TAScam
080300061122     c                   movel     INDcit        TASlom
080400061122     c                   movel     INDprv        TASprm
080500061122     c                   movel     INDsta        TASnzm
080600061122e   3c                   endif
080700061121
080800061121     c                   endsr
080900061122     C*------------------------------------------------------------------------*
081000061122     C*? RECIVA - RECUPERO DA ANAGRAFICO CLIENTE IVA E/O CODICE FISCALE
081100061122     C*------------------------------------------------------------------------*
081200061122     C     RECIVA        BEGSR
081300061122     C                   clear                   ds_CNACO
081400061122     C                   clear                   ds_CNIND
081500061122     C                   clear                   ds_CNCLP
081600061122     C                   clear                   ds_FNCLS
081700061122     c                   z-add     TASKSC        I69kin
081800061122     c                   call      'TIBS69R'
081900061122     c                   parm                    TIBS69ds
082000061122     c                   parm                    ds_CNACO
082100061122     c                   parm                    ds_CNIND
082200061122     c                   parm                    ds_CNCLP
082300061122     c                   parm                    ds_FNCLS
082400061122
082500061122     c                   endsr
082600990802     C*------------------------------------------------------------------------*
082700990802     C*? SETTAT - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAT - SEGNACOLLI
082800990802     C*------------------------------------------------------------------------*
082900990802     C     SETTAT        BEGSR
083000990802     C*
083100990802     C* AZZERO CONTATORE RECORD LETTI
083200990802     C                   MOVEL     '0'           FINTAT            1
083300990802     C*
083400990802     C* POSIZIONAMENTO E PRIMA LETTURA
083500990802     C                   Z-ADD     TASAAS        TATAAS
083600990802     C                   Z-ADD     TASLNP        TATLNP
083700990802     C                   Z-ADD     TASNRS        TATNRS
083800990802     C                   Z-ADD     TASNSP        TATNSP
083900001019     C     KEYTAT        CHAIN     TITAT30C                           99        *FINE-ARCHIV
084000990802     C     *IN99         IFEQ      '1'
084100990802     C                   MOVEL     '1'           FINTAT
084200001019     C                   ENDIF
084300990802     C*
084400990802     C                   ENDSR
084500990802     C*------------------------------------------------------------------------*
084600990802     C*? LETTAT - LETTURA PROSSIMO RECORD TITAT - SEGNACOLLI
084700990802     C*------------------------------------------------------------------------*
084800990802     C     LETTAT        BEGSR
084900010124     C*
085000010124     C                   READ      TITAT30C                               99
085100990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
085200010124     C     TATAAS        ORNE      TASAAS
085300010124     C     TATLNP        ORNE      TASLNP
085400010124     C     TATNRS        ORNE      TASNRS
085500010124     C     TATNSP        ORNE      TASNSP
085600990802     C                   MOVEL     '1'           FINTAT
085700001019     C                   ENDIF
085800990802     C*
085900990802     C                   ENDSR
086000990802     C*------------------------------------------------------------------------*
086100990802     C*? SETTA4 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA4 - RIFERIMENTI
086200990802     C*------------------------------------------------------------------------*
086300990802     C     SETTA4        BEGSR
086400990802     C*
086500990802     C* AZZERO CONTATORE RECORD LETTI
086600990802     C                   MOVEL     '0'           FINTA4            1
086700990802     C*
086800990802     C* POSIZIONAMENTO E PRIMA LETTURA
086900990802     C                   Z-ADD     TASAAS        TA4AAS
087000990802     C                   Z-ADD     TASLNP        TA4LNP
087100990802     C                   Z-ADD     TASNRS        TA4NRS
087200990802     C                   Z-ADD     TASNSP        TA4NSP
087300001019     C     KEYTA4        CHAIN     TITA430C                           99        *FINE-ARCHIV
087400990802     C     *IN99         IFEQ      '1'
087500990802     C                   MOVEL     '1'           FINTA4
087600061122      * recupero riferimento mittente alfanumerico
087700061122     c                   else
087800061122     c                   exsr      recta4
087900001019     C                   ENDIF
088000990802     C*
088100990802     C                   ENDSR
088200990802     C*------------------------------------------------------------------------*
088300990802     C*? LETTA4 - LETTURA PROSSIMO RECORD TITA4 - RIFERIMENTI
088400990802     C*------------------------------------------------------------------------*
088500990802     C     LETTA4        BEGSR
088600010124     C*
088700010124     C                   READ      TITA430C                               99
088800990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
088900010124     C     TA4AAS        ORNE      TASAAS
089000010124     C     TA4LNP        ORNE      TASLNP
089100010124     C     TA4NRS        ORNE      TASNRS
089200010124     C     TA4NSP        ORNE      TASNSP
089300990802     C                   MOVEL     '1'           FINTA4
089400001019     C                   ENDIF
089500061122      * se letto
089600061123     c                   if        finta4 = '0'
089700061122     c                   exsr      recta4
089800061122     c                   endif
089900990802     C*
090000990802     C                   ENDSR
090100061122     C*------------------------------------------------------------------------*
090200061122     C*? RECTA4 - RECUPERO RIFERIMENTO MITTENTE ALFANUMERICO / CODICE FISCALE
090300061122     C*------------------------------------------------------------------------*
090400061122     C     RECTA4        BEGSR
090500061122      * riferimento mittente alfanumerico
090600061122     c                   IF        ta4trc = 'A'
090700061122     c                   movel     ta4not        dta4a
090800061122     c                   movel     §ta4aRMA      Tasrma
090900061122     c                   endif
091000061122      * CODICE FISCALE
091100061122     c                   IF        ta4trc = 'Q'
091200061122     c                   movel     ta4not        dta4q
091300061122      * VERIFICO SE FRANCO O ASSEGNATO
091400061122     c                   If        %subst(tastbl:1:1) = 'F'
091500061122     c                   movel     §TA4QCFM      Tascdf
091600061122     c                   endif
091700061122     c                   If        %subst(tastbl:1:1) = 'A'
091800061122     c                   movel     §TA4QCFD      Tascdf
091900061122     c                   endif
092000061122     c                   endif
092100061122
092200061122     c                   endsr
092300010903     C*------------------------------------------------------------------------*
092400010903     C*? SETTAH - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
092500010903     C*------------------------------------------------------------------------*
092600010903     C     SETTAH        BEGSR
092700010903     C*
092800010903     C* AZZERO CONTATORE RECORD LETTI
092900010903     C                   MOVEL     '0'           FINTAH            1
093000010903     C*
093100010903     C* POSIZIONAMENTO E PRIMA LETTURA
093200010903     C                   Z-ADD     TASAAS        TAHAAS
093300010903     C                   Z-ADD     TASLNP        TAHLNP
093400010903     C                   Z-ADD     TASNRS        TAHNRS
093500010903     C                   Z-ADD     TASNSP        TAHNSP
093600010903     C     KEYTAH        CHAIN     TITAH30C                           99        *FINE-ARCHIV
093700010903     C     *IN99         IFEQ      '1'
093800010903     C                   MOVEL     '1'           FINTAH
093900010903     C                   ENDIF
094000010903     C*
094100010903     C                   ENDSR
094200010903     C*------------------------------------------------------------------------*
094300010903     C*? LETTAH - LETTURA PROSSIMO RECORD TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
094400010903     C*------------------------------------------------------------------------*
094500010903     C     LETTAH        BEGSR
094600010903     C*
094700010903     C                   READ      TITAH30C                               99
094800010903     C     *IN99         IFEQ      '1'                                          *FINE FILE
094900010903     C     TAHAAS        ORNE      TASAAS
095000010903     C     TAHLNP        ORNE      TASLNP
095100010903     C     TAHNRS        ORNE      TASNRS
095200010903     C     TAHNSP        ORNE      TASNSP
095300010903     C                   MOVEL     '1'           FINTAH
095400010903     C                   ENDIF
095500010903     C*
095600010903     C                   ENDSR
095700950307     C*------------------------------------------------------------------------*
095800950307     C*? SETCED - POSIZIONAMENTO E PRIMA LETTURA DEL FILE ECCED
095900950307     C*------------------------------------------------------------------------*
096000950307     C     SETCED        BEGSR
096100950307     C*
096200950307     C* AZZERO CONTATORE RECORD LETTI
096300950307     C                   MOVEL     '0'           FINCED            1
096400950307     C*
096500950307     C* POSIZIONAMENTO E PRIMA LETTURA
096600950307     C                   Z-ADD     TASAAS        CEDAAS
096700950307     C                   Z-ADD     TASLNP        CEDLNP
096800950307     C                   Z-ADD     TASNRS        CEDNRS
096900950307     C                   Z-ADD     TASNSP        CEDNSP
097000950307     C                   MOVEL     TASTBL        CEDTBL
097100001019     C     KEYCED        CHAIN     ECCED30C                           99        *FINE-ARCHIV
097200950307     C     *IN99         IFEQ      '1'
097300950307     C                   MOVEL     '1'           FINCED
097400001019     C                   ENDIF
097500950307     C*
097600950307     C                   ENDSR
097700950307     C*------------------------------------------------------------------------*
097800950307     C*? LETCED - LETTURA PROSSIMO RECORD ECCED
097900950307     C*------------------------------------------------------------------------*
098000950307     C     LETCED        BEGSR
098100950307     C*
098200010124     C                   READ      ECCED30C                               99
098300950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
098400010124     C     CEDAAS        ORNE      TASAAS
098500010124     C     CEDLNP        ORNE      TASLNP
098600010124     C     CEDNRS        ORNE      TASNRS
098700010124     C     CEDNSP        ORNE      TASNSP
098800010124     C     CEDTBL        ORNE      TASTBL
098900950307     C                   MOVEL     '1'           FINCED
099000001019     C                   ENDIF
099100950307     C*
099200950307     C                   ENDSR
099300950307     C*------------------------------------------------------------------------*
099400990802     C*? CHKTAS - CONTROLLA VALIDITA' DEL RECORD DI TITAS
099500950307     C*------------------------------------------------------------------------*
099600950309     C     CHKTAS        BEGSR
099700950307     C*
099800950307     C                   MOVEL     'S'           WRECOK            1
099900000329     C*
100000001023     C* CONTROLLA LO STATO DELLA CONSEGNA IN RELAZIONE AL TIPO BOLLA
100100001023     C* ==> SE TRATTASI DI BOLLA DI RECUPERO (F3/A3/AP) STORICIZZO COMUNQUE
100200001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
100300001018     C     TASDCM        IFEQ      *ZEROS                                       *NON CONSEGNATA
100400001023     C     TASTBL        ANDNE     'F3'
100500001023     C     TASTBL        ANDNE     'A3'
100600001023     C     TASTBL        ANDNE     'AP'
100700010222     C     TASCBO        ANDNE     'E '
100800010222     C     TASCBO        ANDNE     'B '
100900001019     C                   EXCEPT    ERRDCM
101000000329     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
101100001018     C                   ENDIF
101200001018     C                   ENDIF
101300950309     C*
101400950309     C* CONTROLLA IL CONTRASSEGNO
101500001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
101600950309     C     TASFCA        IFEQ      'S'                                          --- 1 -->
101700950309     C                   Z-ADD     TASAAS        CSBAAS
101800950309     C                   Z-ADD     TASLNP        CSBLNP
101900950309     C                   Z-ADD     TASNRS        CSBNRS
102000950309     C                   Z-ADD     TASNSP        CSBNSP
102100950309     C     KEYCSB        CHAIN     TNCSB03L                           99
102200950309     C     *IN99         IFEQ      '0'                                          --- 2 -->
102300950309     C     CSBSTA        IFNE      9                                            --- 3 -->
102400950309     C     CSBBNA        IFEQ      *ZEROS                                       *NON ANCORA PAGATO
102500950309     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
102600021021     C***                EXCEPT    NOCCSB                                       *STAMPO RECORD
102700021021     C                   ADD       1             CNTCSB                         *CONTEGGIO RECORD
102800950309     C                   ENDIF
102900950309     C                   ENDIF                                                  <-- 3 ---
103000950309     C                   ENDIF                                                  <-- 2 ---
103100950309     C                   ENDIF                                                  <-- 1 ---
103200001018     C                   ENDIF                                                  <-- 1 ---
103300000327     C*
103400000327     C* CONTROLLA I DANNI
103500001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
103600001018     C     TASFDA        IFEQ      'S'
103700040715      **
103800040715      **  Sostituito con il pgm FIDN12R che restituisce l'informazione
103900040715      **   sui danni fra bolla madre e figlia
104000040715      **
104100040715     C**                 Z-ADD     TASAAS        KCTAAS
104200040715     C**                 Z-ADD     TASLNP        KCTLNP
104300040715     C**                 Z-ADD     TASNRS        KCTNRS
104400040715     C**                 Z-ADD     TASNSP        KCTNSP
104500040715     C**   KEYDCT        CHAIN     FNDCT02L                           99
104600040715      *
104700040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
104800040715      * SIA COME MAMMA CHE COME FIGLIA
104900040715     c                   clear                   fidn12ds
105000040715     c                   eval      i12aas = TASAAS
105100040715     c                   eval      i12lnp = TASLNP
105200040715     c                   eval      i12nrs = TASNRS
105300040715     c                   eval      i12nsp = TASNSP
105400040715     c                   eval      i12fel = 'E'
105500040715     c                   call      'FIDN12R'
105600040715     c                   parm                    fidn12ds
105700040715      **
105800040715      * se c'è almeno una C.A. aperta
105900040715      **
106000040715     c     o12nca        ifgt      0
106100040715     C**   *IN99         IFEQ      *OFF                                         *TROVATO DANNO
106200040715      **
106300000327     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
106400021021     C***                EXCEPT    NOCDCT                                       *STAMPO RECORD
106500021021     C                   ADD       1             CNTDCT                         *CONTEGGIO RECORD
106600001018     C                   ENDIF
106700001018     C                   ENDIF
106800001018     C                   ENDIF
106900000404     C*
107000000404     C* CONTROLLA LE GIACENZE
107100001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
107200001018     C     TASFGC        IFEQ      'S'
107300000404     C                   Z-ADD     TASAAS        KCPAAS
107400000404     C                   Z-ADD     TASLNP        KCPLNP
107500000404     C                   Z-ADD     TASNRS        KCPNRS
107600000404     C                   Z-ADD     TASNSP        KCPNSP
107700000404     C                   Z-ADD     0             KCPFRG                         *PRIMO PRG
107800050301     C     KEYGCP        CHAIN(N)  TIGCP51L                           99
107900001018     C     *IN99         IFEQ      *OFF
108000001018     C     GCPFAS        IFLT      50                                           *FASI APERTE
108100000404     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
108200021021     C***                EXCEPT    NOCGCP                                       *STAMPO RECORD
108300021021     C                   ADD       1             CNTGCP                         *CONTEGGIO RECORD
108400001018     C                   ENDIF
108500001018     C                   ENDIF
108600001018     C                   ENDIF
108700001018     C                   ENDIF
108800011227     C*
108900020220     C* SE BOLLA POSTA CANCELLARE SOLO SE + VECCHIA D 2 ANNI
109000011227     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
109100011227     C     TASTSP        IFEQ      'P'                                          *BOLLA POSTA
109200020220     C     TASDSP        ANDGT     DSDATFP
109300011227     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
109400011227     C                   ENDIF
109500011227     C                   ENDIF
109600001019     C*
109700140109     C* DEVE Controllare: in caso di RESO controlla se la figlia è stata consegnata
109800140109     C*  altrimenti NON deve cancellare la MAMMA.
109900140109     C*
110000131216     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
110100131216     C                   IF        tasCCA = '2' or tasCCA = '6'                 *BOLLA RESO
110200131216     **
110300131223     C* Chiamata metodo GetLastChild
110400131223     c                   exsr      CHKCONS
110500140102     C                   IF         Consegnata_OK <> 'S'
110600140102     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
110700140102     C                   ENDIF
110800131223     **
110900131216     C                   ENDIF
111000131216     C                   ENDIF
111100131216     C*
111200131216     C*
111300131216     C*
111400001019     C* CONTEGGIO BOLLE SCARTATE
111500001019     C     WRECOK        IFEQ      'N'
111600001019     C                   ADD       1             NUMNON
111700001019     C                   ENDIF
111800950307     C*
111900950307     C                   ENDSR
112000131223     C*------------------------------------------------------------------------*
112100131223     C* CHKCONS  - Verifica se bolla effettivamente consegnata
112200131223     C*------------------------------------------------------------------------*
112300131223     C     CHKCONS       BEGSR
112400131223     C*
112500140102     C                   movel     'N'           Consegnata_OK     1
112600131223     C                   movel     *blanks       wEsito1           1
112700131223     C                   movel     *blanks       wEsito2           1
112800131223     C*
112900131223     C                   eval      wAAS = tasAAS
113000131223     C                   eval      wLNP = tasLNP
113100131223     C                   eval      wNRS = tasNRS
113200131223     C                   eval      wNSP = tasNSP
113300131223     C*
113400131223     C* Chiamata metodo GetLblTyp
113500131223     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
113600131223     C                                                wAAS
113700131223     C                                               :wLNP
113800131223     C                                               :wNRS
113900131223     C                                               :wNSP
114000131223     C                                               :pOutLblTyp
114100131223     C                                               :pOutAnnoBO
114200131223     C                                               :pOutLineaParBO
114300131223     C                                               :pOutSerieBO
114400131223     C                                               :pOutNumSpedBO
114500131223     C                                               :pOutDcmBO
114600131223     C                                               :pOutCcaBO
114700131223     C                                               :pOutRblBO))
114800131223     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
114900131223     C                   if        pOutLblTyp <> 'O'
115000131223     C                   eval      wAAS = pOutAnnoBO
115100131223     C                   eval      wLNP = pOutLineaParBO
115200131223     C                   eval      wNRS = pOutSerieBO
115300131223     C                   eval      wNSP = pOutNumSpedBO
115400131223     C                   endif
115500131223     C*
115600131223     C* Chiamata metodo GetLastChild
115700131223     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
115800131223     C                                                wAAS
115900131223     C                                               :wLNP
116000131223     C                                               :wNRS
116100131223     C                                               :wNSP
116200131223     C                                               :pOutAnnoFI
116300131223     C                                               :pOutLineaParFI
116400131223     C                                               :pOutSerieFI
116500131223     C                                               :pOutNumSpedFI
116600131223     C                                               :pOutDcmFI
116700131223     C                                               :pOutCcaFI))
116800131223     C*
116900140102     C* Considerazioni sullo stato di Consegna della bolla
117000140102     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)
117100140102     C                               or
117200131223     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
117300140102     C                   movel     'S'           Consegnata_OK     1
117400131223     C                   endif
117500131223     C*
117600131223     C                   ENDSR
117700950309     C*------------------------------------------------------------------------*
117800950309     C*?AGGTAB - AGGIORNA LA TABELLA '$3' CON ULTIMA DATA TRASF. TAS00 > TAS10
117900950309     C*------------------------------------------------------------------------*
118000950309     C     AGGTAB        BEGSR
118100950309     C*
118200950309     C                   OPEN      TABEL00F
118300950309     C*
118400950309     C                   Z-ADD     1             KTBUTE
118500950309     C                   MOVEL     '$3'          KTBCOD
118600950309     C                   MOVEL     '1'           KTBKEY
118700950309     C     KEYTAB        CHAIN     TABEL00F                           99
118800950309     C     *IN99         IFEQ      *ON                                          *NON TROVATA
118900950309     C                   ELSE                                                   *TROVATA
119000950309     C                   MOVEL     TBLUNI        DS$3
119100950309     C                   Z-ADD     PARDTF        §$3DP0                         *ULT.DATA TRASF P>SA
119200020220     C                   Z-ADD     PARDFP        §$3DPP                         *ULT.DATA TRASF P>SA
119300950309     C                   MOVEL     DS$3          TBLUNI
119400950309     C                   UPDATE    TABEL
119500950309     C                   ENDIF
119600950309     C*
119700950309     C                   CLOSE     TABEL00F
119800950309     C*
119900950309     C                   ENDSR
120000950717     C*------------------------------------------------------------------------*
120100950717     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
120200950717     C*------------------------------------------------------------------------*
120300950717     C     SUBDAT        BEGSR
120400950717     C                   TIME                    DTTIME
120500950717     C                   Z-ADD     DTANN         DSANN
120600950717     C                   Z-ADD     DTMES         DSMES
120700950717     C                   Z-ADD     DTGIO         DSGIO
120800950717     C                   Z-ADD     DTSEC         DSSEC
120900950717     C                   ENDSR
121000940912     C*------------------------------------------------------------------------*
121100050705     C*? funzione per scivere file x Laguna
121200940912     C*------------------------------------------------------------------------*
121300050705     C     srlaguna      BEGSR
121400050705     c                   clear                   tnsb34ds
121500050929     c* ricavo il progressivo da impostare nella directory ogni 450000 bolle
121600050720     c* cancellate cambio directory
121700120709      * 09/07/2012 con Bocchi verificato che il progressivo non serve più
121800120709     C**!!!              ADD       1             NUMlag                         *REC TRASF TITAS
121900120709     c**!!!numlag        div       450000        sb34prg
122000120709     c**!!!              add       1             sb34prg
122100050705     c                   eval      sb34tla = 'R'
122200050705     c                   eval      sb34aas = tasaas
122300050705     c                   eval      sb34lnp = taslnp
122400050705     c                   eval      sb34nrs = tasnrs
122500050705     c                   eval      sb34nsp = tasnsp
122600050706     c                   eval      sb34dtp = pardtf
122700050706     c                   eval      sb34cop = parcop
122800050705     c                   call      'TNSB34R'
122900050705     c                   parm                    tnsb34ds
123000050705     c                   if        sb34err <> ' '
123100050705     c                   end
123200050705     C                   ENDSR
123300120709     C*------------------------------------------------------------------------*
123400120709     C*? funzione per scivere file x JDOC
123500120709     C*------------------------------------------------------------------------*
123600120709     C     srJdoc        BEGSR
123700120709     c                   clear                   tnsb34ds
123800120709     c                   eval      sb34tla = 'R'
123900120709     c                   eval      sb34aas = tasaas
124000120709     c                   eval      sb34lnp = taslnp
124100120709     c                   eval      sb34nrs = tasnrs
124200120709     c                   eval      sb34nsp = tasnsp
124300120709     c                   eval      sb34dtp = pardtf
124400120709     c                   eval      sb34cop = parcop
124500120709     c                   call      'TNSB34R1'
124600120709     c                   parm                    tnsb34ds
124700120709     c                   if        sb34err <> ' '
124800120709     c                   end
124900120709     C                   ENDSR
125000061122     C*------------------------------------------------------------------------*
125100061122     C*? WRTTASSTO - SCRITTURA FILE STORICO TITASS
125200061122     C*------------------------------------------------------------------------*
125300061122     C     WRTTASSTO     BEGSR
125400080312     c* memorizzo directory laguna per sapere dove è stata archiviata la spe
125500080312     c                   clear                   tasdtfp
125600080312     c                   if        §floiml <> ' '
125700080312     c                   move      tasaas        ladaas
125800080312     c                   move      taslnp        ladlnp
125900080312     c                   move      tasnrs        ladnrs
126000080312     c                   move      tasnsp        ladnsp
126100080312     c     klad          chain     tilad01l
126200080312     c                   If        %found(tilad01l)
126300080312     c                   movel     laddir        tasdtfp
126400080312     c                   end
126500080312     c                   end
126600061122      * Decodifica padroncino
126700061122     c                   If        taspdc <> *zeros
126800061122     c     Keyapd        chain     fiapd01l
126900061122     c                   If        %found(fiapd01l)
127000061122     c                   eval      taspdd = apdrsc
127100061122     c                   endif
127200061122     c                   endif
127300061122
127400061122      * recupero c/assegno
127500061122     c                   if        tasfca = 'S'
127600061122     c                   z-add     csbcas        Tascas
127700061122     c                   movel     csbvca        tasvca
127800061122     c                   endif
127900061122
128000061122     c                   move      tasksc        com04
128100061122      * PARTITA IVA / CODICE FISCALE
128200061122     c                   If        %subst(tastbl:1:1) = 'A'  and
128300061122     c                             tascpd <> *blanks
128400061122     c                   eval      tasiva  = tascpd
128500061122     c                   endif
128600061122      * se partita iva non valorizzata
128700061122     c                   If        tasiva = *blanks  or tascdf = *blanks
128800061122      * codificato franco o assegnato
128900061122     c                   if        com04  <>  9999  and com04  <>  8888
129000061122     c                   exsr      reciva
129100061122     c                   if        o69err <> *on
129200061122     c                   if        tascdf = *blanks
129300061122     c                   eval      tasiva = indiva
129400061122     c                   endif
129500061122      * se devo cercare anche il codife fiscale
129600061122     c                   if        tascdf = *blanks
129700061122     c                   eval      tascdf = indcdf
129800061122     c                   endif
129900061122
130000061122     c                   endif
130100061122     c                   endif
130200061122     c                   endif
130300061124      * valorizzo il flag che mi identifica le spedizioni senza segnacollo
130400091229     c                   If        Tasfns <> 'S' and tasncd = *zeros
130500061124     c                   eval      tassss = 'S'
130600061124     c                   endif
130700061122
130800061122     c                   write     titassto                             99
130900061122     C   99              EXCEPT    ERRsto
131000061122     C   99              MOVEL     '1'           FINL00                         *FINE PGM
131100061122     C  N99              ADD       1             NUMsto                         *REC TRASF FNEVB
131200061122
131300061122     C                   ENDSR
131400061122     C*
131500050705     C*------------------------------------------------------------------------*
131600050705     C*? RUTINZ - ROUTINE INIZIALE
131700050705     C*------------------------------------------------------------------------*
131800050705     C     RUTINZ        BEGSR
131900931115     C*
132000931115     C* PARAMETRI IN INPUT AL PGM
132100931115     C     *ENTRY        PLIST
132200931115     C                   PARM                    KPJBA
132300931115     C                   MOVEL     KPJBU         PARAM
132400950309     C*
132500950309     C* DEFINISCO CAMPI
132600950309     C     *LIKE         DEFINE    TBLKUT        KTBUTE
132700950309     C     *LIKE         DEFINE    TBLCOD        KTBCOD
132800950309     C     *LIKE         DEFINE    TBLKEY        KTBKEY
132900970903     C     *LIKE         DEFINE    CETNBC        MINNBC
133000040715     C***  *LIKE         DEFINE    DCTAAS        KCTAAS                         *FNDCT00F
133100040715     C***  *LIKE         DEFINE    DCTLNP        KCTLNP
133200040715     C***  *LIKE         DEFINE    DCTNRS        KCTNRS
133300040715     C***  *LIKE         DEFINE    DCTNSP        KCTNSP
133400050301     C     *LIKE         DEFINE    GCPAAS        KCPAAS                         *TIGCP00F
133500000404     C     *LIKE         DEFINE    GCPLNP        KCPLNP
133600000404     C     *LIKE         DEFINE    GCPNRS        KCPNRS
133700000404     C     *LIKE         DEFINE    GCPNSP        KCPNSP
133800000404     C     *LIKE         DEFINE    GCPFRG        KCPFRG
133900050301     C     *LIKE         DEFINE    GAPAAS        KAPAAS                         *TIGAP00F
134000000404     C     *LIKE         DEFINE    GAPLNP        KAPLNP
134100000404     C     *LIKE         DEFINE    GAPNRS        KAPNRS
134200000404     C     *LIKE         DEFINE    GAPNSP        KAPNSP
134300050301     C     *LIKE         DEFINE    GNPAGC        KNPAGC                         *TIGNP00F
134400000404     C     *LIKE         DEFINE    GNPFGC        KNPFGC
134500000404     C     *LIKE         DEFINE    GNPNGC        KNPNGC
134600950309     C*
134700950309     C* CHIAVE LETTURA TABEL00F - COMPLETA
134800950309     C     KEYTAB        KLIST
134900950309     C                   KFLD                    KTBUTE
135000950309     C                   KFLD                    KTBCOD
135100950309     C                   KFLD                    KTBKEY
135200080925     C     KEYTAB1       KLIST
135300080925     C                   KFLD                    KTBUTE
135400080925     C                   KFLD                    KTBCOD
135500080925     C                   Z-ADD     1             KTBUTE
135600080925     C                   MOVEL     'TB'          KTBCOD
135700950309     C*
135800950309     C* CHIAVE LETTURA TNCSB01L - COMPLETA
135900950309     C     KEYCSB        KLIST
136000950309     C                   KFLD                    CSBAAS
136100950309     C                   KFLD                    CSBLNP
136200950309     C                   KFLD                    CSBNRS
136300950309     C                   KFLD                    CSBNSP
136400000327     C*
136500000327     C* CHIAVE LETTURA FNDCT02L - COMPLETA
136600040715     C***  KEYDCT        KLIST
136700040715     C***                KFLD                    KCTAAS
136800040715     C***                KFLD                    KCTLNP
136900040715     C***                KFLD                    KCTNRS
137000040715     C***                KFLD                    KCTNSP
137100930208     C*
137200990802     C* CHIAVE LETTURA TITAS38C - PARZIALE
137300930621     C     KEYTAS        KLIST
137400941123     C                   KFLD                    TASAAS
137500941123     C                   KFLD                    TASMGS
137600950307     C*
137700990802     C* CHIAVE LETTURA TITA730C - COMPLETA
137800950307     C     KEYTA7        KLIST
137900950307     C                   KFLD                    TA7AAS
138000950307     C                   KFLD                    TA7LNP
138100950307     C                   KFLD                    TA7NRS
138200950307     C                   KFLD                    TA7NSP
138300950307     C                   KFLD                    TA7TBL
138400990802     C*
138500990802     C* CHIAVE LETTURA TITAA30C - COMPLETA
138600990802     C     KEYTAA        KLIST
138700990802     C                   KFLD                    TAAAAS
138800990802     C                   KFLD                    TAALNP
138900990802     C                   KFLD                    TAANRS
139000990802     C                   KFLD                    TAANSP
139100990802     C*
139200990802     C* CHIAVE LETTURA TITAT30C - COMPLETA
139300990802     C     KEYTAT        KLIST
139400990802     C                   KFLD                    TATAAS
139500990802     C                   KFLD                    TATLNP
139600990802     C                   KFLD                    TATNRS
139700990802     C                   KFLD                    TATNSP
139800990802     C*
139900990802     C* CHIAVE LETTURA TITA430C - COMPLETA
140000990802     C     KEYTA4        KLIST
140100990802     C                   KFLD                    TA4AAS
140200990802     C                   KFLD                    TA4LNP
140300990802     C                   KFLD                    TA4NRS
140400990802     C                   KFLD                    TA4NSP
140500010903     C*
140600010903     C* CHIAVE LETTURA TITAH30C - COMPLETA
140700010903     C     KEYTAH        KLIST
140800010903     C                   KFLD                    TAHAAS
140900010903     C                   KFLD                    TAHLNP
141000010903     C                   KFLD                    TAHNRS
141100010903     C                   KFLD                    TAHNSP
141200950209     C*
141300950307     C* CHIAVE LETTURA ECCET30C - COMPLETA
141400950220     C     KEYCET        KLIST
141500950220     C                   KFLD                    CETAAS                         ANNO SPEDIZIONE
141600950220     C                   KFLD                    CETLNP                         LINEA DI PARTENZA
141700950220     C                   KFLD                    CETNRS                         NUMERO SERIE
141800950220     C                   KFLD                    CETNSP                         NUMERO SPEDIZIONE
141900950220     C                   KFLD                    CETTBL                         TIPO BOLLA
142000950307     C*
142100950307     C* CHIAVE LETTURA ECCED30C - COMPLETA
142200950307     C     KEYCED        KLIST
142300950307     C                   KFLD                    CEDAAS                         ANNO SPEDIZIONE
142400950307     C                   KFLD                    CEDLNP                         LINEA DI PARTENZA
142500950307     C                   KFLD                    CEDNRS                         NUMERO SERIE
142600950307     C                   KFLD                    CEDNSP                         NUMERO SPEDIZIONE
142700950307     C                   KFLD                    CEDTBL                         TIPO BOLLA
142800980325     C*
142900980325     C* CHIAVE LETTURA ECFIM30C - COMPLETA
143000980325     C     KEYFIM        KLIST
143100980325     C                   KFLD                    FIMAAS                         ANNO SPEDIZIONE
143200980325     C                   KFLD                    FIMLNP                         LINEA DI PARTENZA
143300980325     C                   KFLD                    FIMNRS                         NUMERO SERIE
143400980325     C                   KFLD                    FIMNSP                         NUMERO SPEDIZIONE
143500980325     C                   KFLD                    FIMTBL                         TIPO BOLLA
143600000327     C*
143700000327     C* CHIAVE LETTURA FNEVB01L - COMPLETA
143800000327     C     KEYEVB        KLIST
143900000327     C                   KFLD                    EVBAAS                         ANNO SPEDIZIONE
144000000327     C                   KFLD                    EVBLNP                         LINEA DI PARTENZA
144100000327     C                   KFLD                    EVBNRS                         NUMERO SERIE
144200000327     C                   KFLD                    EVBNSP                         NUMERO SPEDIZIONE
144300000404     C*
144400050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
144500000404     C     KEYGCP        KLIST
144600000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
144700000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
144800000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
144900000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
145000000404     C                   KFLD                    KCPFRG                         PRG RIAPERTURE
145100000404     C*
145200050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
145300000404     C     KE2GCP        KLIST
145400000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
145500000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
145600000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
145700000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
145800000404     C*
145900050301     C* CHIAVE LETTURA TIGAP01L - PARZIALE
146000000404     C     KEYGAP        KLIST
146100000404     C                   KFLD                    KAPAAS                         ANNO SPEDIZIONE
146200000404     C                   KFLD                    KAPLNP                         LINEA DI PARTENZA
146300000404     C                   KFLD                    KAPNRS                         NUMERO SERIE
146400000404     C                   KFLD                    KAPNSP                         NUMERO SPEDIZIONE
146500000404     C*
146600050301     C* CHIAVE LETTURA TIGNP01L - PARZIALE
146700000404     C     KEYGNP        KLIST
146800000404     C                   KFLD                    KNPAGC                         ANNO APERTURA
146900000404     C                   KFLD                    KNPFGC                         FILILAE APERTURA
147000000404     C                   KFLD                    KNPNGC                         NUMERO GIACENZA
147100040303     C*
147200040531     C* CHIAVE LETTURA FIAR531C
147300040303     C     KEYAR5        KLIST
147400040303     C                   KFLD                    AR5AAS
147500040303     C                   KFLD                    AR5LNP
147600040303     C                   KFLD                    AR5NRS
147700040303     C                   KFLD                    AR5NSP
147800120709
147900120709     C     KEYAR51       KLIST
148000120709     C                   KFLD                    AR5AAS
148100120709     C                   KFLD                    AR5LNP
148200120709     C                   KFLD                    AR5NRS
148300120709     C                   KFLD                    AR5NSP
148400120709     C                   KFLD                    AR5TRD
148500060919     C*
148600060919     C* CHIAVE LETTURA TITAI01L
148700060919     C     KEYtai        KLIST
148800060919     C                   KFLD                    taiAAS
148900060919     C                   KFLD                    taiLNP
149000060919     C                   KFLD                    taiNRS
149100060919     C                   KFLD                    taiNSP
149200060919     C                   KFLD                    taiTBL
149300081103
149400081103     C* CHIAVE LETTURA TITAI01L
149500081103     C     KEYrab        KLIST
149600081103     C                   KFLD                    rabAAS
149700081103     C                   KFLD                    rabLNP
149800081103     C                   KFLD                    rabNRS
149900081103     C                   KFLD                    rabNSP
150000061122
150100061122     C* CHIAVE LETTURA Fiapd01l
150200061122     C     KEYapd        KLIST
150300061122     C                   KFLD                    trcapd
150400061122     C                   KFLD                    taspdc
150500040823      *
150600040823     C     kbol          klist
150700040823     C                   kfld                    tasaas
150800040823     C                   kfld                    taslnp
150900040823     C                   kfld                    tasnrs
151000040823     C                   kfld                    tasnsp
151100080312      *
151200080312     C     klad          klist
151300080312     C                   kfld                    LADAAS
151400080312     C                   kfld                    LADLNP
151500080312     C                   kfld                    LADNRS
151600080312     C                   kfld                    LADNSP
151700000327     C*
151800131029     C     Kevb2f        KLIST
151900131029     C                   KFLD                    EVBAAS
152000131029     C                   KFLD                    EVBLNP
152100131029     C                   KFLD                    EVBNRS
152200131029     C                   KFLD                    EVBNSP
152300131029     C                   KFLD                    EVBDTV
152400131029     C                   KFLD                    EVBORV
152500131029     C                   KFLD                    EVBCEV
152600000327     C* CALCOLA LA DATA CORRENTE
152700000327     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
152800000327     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
152900000327     C                   Z-ADD     N8            G08DAT
153000000327     C                   Z-ADD     *ZEROS        G08INV
153100000327     C                   MOVEL     '0'           G08ERR
153200000327     C                   CALL      'XSRDA8'
153300000327     C                   PARM                    WLBDA8
153400000327     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
153500930208     C*
153600930208     C* IMPOSTO A ZERO IL FINE PGM
153700930208     C                   MOVEL     '0'           FINL00            1
153800931105     C                   MOVEL     *BLANKS       RSUT             20
153900040922     C                   Z-ADD     *ZEROS        NUMblp            9 0
154000040922     C                   Z-ADD     *ZEROS        NUMarb            9 0
154100950308     C                   Z-ADD     *ZEROS        NUMTAS            9 0
154200120709     C**!!!              Z-ADD     *ZEROS        NUMlag            9 0
154300950308     C                   Z-ADD     *ZEROS        NUMTA7            9 0
154400990802     C                   Z-ADD     *ZEROS        NUMTAA            9 0
154500990802     C                   Z-ADD     *ZEROS        NUMTAT            9 0
154600990802     C                   Z-ADD     *ZEROS        NUMTA4            9 0
154700010903     C                   Z-ADD     *ZEROS        NUMTAH            9 0
154800950307     C                   Z-ADD     *ZEROS        NUMCET            9 0
154900950308     C                   Z-ADD     *ZEROS        NUMCED            9 0
155000980325     C                   Z-ADD     *ZEROS        NUMFIM            9 0
155100970904     C                   Z-ADD     *ZEROS        NUMECT            9 0
155200970904     C                   Z-ADD     *ZEROS        NUMECD            9 0
155300000327     C                   Z-ADD     *ZEROS        NUMEVB            9 0
155400000404     C                   Z-ADD     *ZEROS        NUMGCP            9 0
155500000404     C                   Z-ADD     *ZEROS        NUMGAP            9 0
155600000404     C                   Z-ADD     *ZEROS        NUMGNP            9 0
155700001019     C                   Z-ADD     *ZEROS        NUMNON            9 0
155800040303     C                   Z-ADD     *ZEROS        NUMAR5            9 0
155900060919     C                   Z-ADD     *ZEROS        NUMtai            9 0
156000081103     C                   Z-ADD     *ZEROS        NUMrab            9 0
156100061122     C                   Z-ADD     *ZEROS        NUMsto            9 0
156200021021     C                   Z-ADD     *ZEROS        CNTGCP            9 0
156300021021     C                   Z-ADD     *ZEROS        CNTDCT            9 0
156400021021     C                   Z-ADD     *ZEROS        CNTCSB            9 0
156500040823      * Apro il Fnblp di filiale
156600040823     c                   Clear                   comman
156700040823     c                   Movel(p)  cmd(1)        comman
156800040823     c                   Eval      %Subst(comman:13:8) = 'FNBLP01L'
156900040910      *test sistema informativo di prova
157000040910     c                   if        %subst(knsif:7:1) = 'P'
157100040910     c                   Eval      %Subst(comman:30:10) = wlibp
157200040910     c                   else
157300040910     c                   Eval      %Subst(comman:30:10) = wlib
157400040910     c                   endif
157500040910
157600040823     c                   Eval      comman =
157700040823     c                             %trim(comman) + '/FNBLP01L)'
157800040823     c                   Call      'QCMDEXC'                            99
157900040823     c                   Parm                    comman
158000040823     c                   Parm                    lenght
158100040823     c                   If        *In99
158200040823     c                   seton                                        lr
158300040823     c                   Else
158400040823     c                   Open      Fnblp01l
158500040823     c                   EndIf
158600040823      *
158700040823      * Apro  Fnarb di filiale
158800040823     c                   Clear                   comman
158900040823     c                   Movel(p)  cmd(1)        comman
159000040823     c                   Eval      %Subst(comman:13:8) = 'FNARB01L'
159100040910      *test sistema informativo di prova
159200040910     c                   if        %subst(knsif:7:1) = 'P'
159300040910     c                   Eval      %Subst(comman:30:10) = wlibp
159400040910     c                   else
159500040910     c                   Eval      %Subst(comman:30:10) = wlib
159600040910     c                   endif
159700040910
159800040823     c                   Eval      comman =
159900040823     c                             %trim(comman) + '/FNARB01L)'
160000040823     c                   Call      'QCMDEXC'                            99
160100040823     c                   Parm                    comman
160200040823     c                   Parm                    lenght
160300040823     c                   If        *In99
160400040823     c                   seton                                        lr
160500040823     c                   Else
160600040823     c                   Open      Fnarb01l
160700040823     c                   EndIf
160800040823      *
160900931115     C*
161000950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
161100080925     c* carico i tipi bolla RECUPERO per evitare di salvarli nel TITASS0F
161200081103     c                   clear                   conta
161300080926     c                   open      tabel00f
161400080925     c     keytab1       setll     tabel00f
161500080925     c                   do        *hival
161600080925     c     keytab1       reade(n)  tabel00f
161700080925     c                   if        %eof(tabel00f)
161800080925     c                   leave
161900080925     c                   end
162000080925     c                   movel(p)  tbluni        dstb
162100080925     c                   if        §TBRBL ='R'
162200080925     c                   add       1             conta             2 0
162300080925     c                   movel     tblkey        tbl(conta)
162400080925     c                   end
162500080925     c                   enddo
162600080926     c                   close     tabel00f
162700000000     C*
162800000000     C                   ENDSR
162900940912     O*------------------------------------------------------------------------*
163000940905     OPRTF198   E            TESTA            02
163100950309     O                                            8 'TNSB38R '
163200931105     O                       RSUT                45
163300990802     O                                           82 'TRASF. TITASP -> SALVAT.'
163400931115     O                                           90 'DAL:'
163500950309     O                       PARDTI              99
163600931115     O                                          110 'AL:'
163700950309     O                       PARDTF             119
163800020220     O                                          128 '/PT'
163900020220     O                       PARDFP             132
164000931105     O                                          150 'Data:'
164100950717     O                       DSDAT6             159 '  /  /  '
164200931105     O                                          167 'Ora:'
164300950717     O                       DTORA6             178 '  .  .  '
164400931105     O                                          190 'Pagina:'
164500931105     O                       PAGE          Z    198
164600931105     O          E            TESTA       1  1
164700931105     O                       TES(2)              66
164800931105     O                       TES(3)             132
164900931105     O                       TES(4)             198
165000931105     O          E            TESTA          1
165100931105     O                       TES(1)              66
165200931105     O                       TES(1)             132
165300931105     O                       TES(1)             198
165400001019     O*------------------------------------
165500001019     O          E            ERRDCM         1
165600001019     O                       TASAAS            +  5
165700001019     O                       TASLNP            +  1
165800001019     O                       TASNRS            +  1
165900001019     O                       TASNSP            +  1
166000001019     O                       TASTBL            +  1
166100001019     O                                         +  1 'BOLLA NON CONS.    '
166200931105     O*------------------------------------
166300950307     O          E            ERRTAS         1
166400950307     O                       TASAAS            +  5
166500950307     O                       TASLNP            +  1
166600950307     O                       TASNRS            +  1
166700950307     O                       TASNSP            +  1
166800950307     O                       TASTBL            +  1
166900990802     O                                         +  1 'ERR. WRITE TITAS   '
167000950307     O*------------------------------------
167100950307     O          E            ERRTA7         1
167200950307     O                       TA7AAS            +  5
167300950307     O                       TA7LNP            +  1
167400950307     O                       TA7NRS            +  1
167500950307     O                       TA7NSP            +  1
167600950307     O                       TA7TBL            +  1
167700950307     O                       TA7SVN            +  1
167800990802     O                                         +  1 'ERR. WRITE TITA7   '
167900990802     O*------------------------------------
168000990802     O          E            ERRTAA         1
168100990802     O                       TAAAAS            +  5
168200990802     O                       TAALNP            +  1
168300990802     O                       TAANRS            +  1
168400990802     O                       TAANSP            +  1
168500990802     O                       TAATRC            +  1
168600990802     O                                         +  1 'ERR. WRITE TITAA   '
168700990802     O*------------------------------------
168800990802     O          E            ERRTAT         1
168900990802     O                       TATAAS            +  5
169000990802     O                       TATLNP            +  1
169100990802     O                       TATNRS            +  1
169200990802     O                       TATNSP            +  1
169300990802     O                       TATNSC            +  1
169400990802     O                                         +  1 'ERR. WRITE TITAT   '
169500990802     O*------------------------------------
169600990802     O          E            ERRTA4         1
169700990802     O                       TA4AAS            +  5
169800990802     O                       TA4LNP            +  1
169900990802     O                       TA4NRS            +  1
170000990802     O                       TA4NSP            +  1
170100990802     O                       TA4TRC            +  1
170200990802     O                                         +  1 'ERR. WRITE TITA4   '
170300010903     O*------------------------------------
170400010903     O          E            ERRTAH         1
170500010903     O                       TAHAAS            +  5
170600010903     O                       TAHLNP            +  1
170700010903     O                       TAHNRS            +  1
170800010903     O                       TAHNSP            +  1
170900010903     O                       TAHTRC            +  1
171000010903     O                                         +  1 'ERR. WRITE TITAH   '
171100950307     O*------------------------------------
171200950307     O          E            ERRCET         1
171300950307     O                       CETAAS            +  5
171400950307     O                       CETLNP            +  1
171500950307     O                       CETNRS            +  1
171600950307     O                       CETNSP            +  1
171700950307     O                       CETTBL            +  1
171800950309     O                                         +  1 'ERR. WRITE ECCET   '
171900950307     O*------------------------------------
172000950307     O          E            ERRCED         1
172100950307     O                       CEDAAS            +  5
172200950307     O                       CEDLNP            +  1
172300950307     O                       CEDNRS            +  1
172400950307     O                       CEDNSP            +  1
172500950307     O                       CEDTBL            +  1
172600950307     O                       CEDCMP            +  1
172700950309     O                                         +  1 'ERR. WRITE ECCED   '
172800980325     O*------------------------------------
172900980325     O          E            ERRFIM         1
173000980325     O                       FIMAAS            +  5
173100980325     O                       FIMLNP            +  1
173200980325     O                       FIMNRS            +  1
173300980325     O                       FIMNSP            +  1
173400980325     O                       FIMTBL            +  1
173500980325     O                                         +  1 'ERR. WRITE ECFIM   '
173600970903     O*------------------------------------
173700970903     O          E            ERRECT         1
173800970903     O                       ECTNBC        Z   +  5
173900970903     O                       ECTCMP        Z   +  1
174000970903     O                       ECTDDC            +  1 '    /  /  '
174100970903     O                                         +  1 'ERR. WRITE ECECT   '
174200970903     O*------------------------------------
174300970903     O          E            ERRECD         1
174400970903     O                       ECDNBC        Z   +  5
174500970903     O                       ECDCMP        Z   +  1
174600970903     O                       ECDFRA            +  1
174700970903     O                       ECDIOE            +  1
174800970903     O                       ECDKCC        Z   +  1
174900970903     O                       ECDKSC        Z   +  1
175000970903     O                       ECDRIC            +  1
175100970903     O                       ECDCCF        Z   +  1
175200970903     O                       ECDDDO            +  1 '    /  /  '
175300970903     O                                         +  1 'ERR. WRITE ECECD   '
175400000404     O*------------------------------------
175500000404     O          E            ERREVB         1
175600000404     O                       EVBAAS            +  5
175700000404     O                       EVBLNP            +  1
175800000404     O                       EVBNRS            +  1
175900000404     O                       EVBNSP            +  1
176000000404     O                                         +  1 'ERR. WRITE FNEVB   '
176100000404     O*------------------------------------
176200000404     O          E            ERRGCP         1
176300000404     O                       GCPAAS            +  5
176400000404     O                       GCPLNP            +  1
176500000404     O                       GCPNRS            +  1
176600000404     O                       GCPNSP            +  1
176700050301     O                                         +  1 'ERR. WRITE TIGCP   '
176800050303     O          E            ERRGCP1        1
176900050303     O                       GCPAAS            +  5
177000050303     O                       GCPLNP            +  1
177100050303     O                       GCPNRS            +  1
177200050303     O                       GCPNSP            +  1
177300050303     O                                         +  1 'GIACENZA ANCORA IN FILIALE'
177400000404     O*------------------------------------
177500000404     O          E            ERRGAP         1
177600000404     O                       GAPAAS            +  5
177700000404     O                       GAPLNP            +  1
177800000404     O                       GAPNRS            +  1
177900000404     O                       GAPNSP            +  1
178000050301     O                                         +  1 'ERR. WRITE TIGAP   '
178100000404     O*------------------------------------
178200000404     O          E            ERRGNP         1
178300000404     O                       GNPAGC            +  5
178400000404     O                       GNPFGC            +  4
178500000404     O                       GNPNGC            +  1
178600050301     O                                         +  1 'ERR. WRITE TIGNP   '
178700040303     O*------------------------------------
178800040303     O          E            ERRAR5         1
178900040303     O                       AR5AAS            +  5
179000040303     O                       AR5LNP            +  1
179100040303     O                       AR5NRS            +  1
179200040303     O                       AR5NSP            +  1
179300040303     O                                         +  1 'ERR. WRITE FIAR5   '
179400060919     O          E            ERRtai         1
179500060919     O                       taiAAS            +  5
179600060919     O                       taiLNP            +  1
179700060919     O                       taiNRS            +  1
179800060919     O                       taiNSP            +  1
179900060919     O                       taitbl            +  1
180000060919     O                                         +  1 'ERR. WRITE TITAI   '
180100081103     O          E            ERRrab         1
180200081103     O                       rabAAS            +  5
180300081103     O                       rabLNP            +  1
180400081103     O                       rabNRS            +  1
180500081103     O                       rabNSP            +  1
180600081103     O                                         +  1 'ERR. WRITE FIRAB   '
180700950309     O*------------------------------------
180800000404     O          E            NOCCSB         1
180900950309     O                       CSBAAS            +  5
181000950309     O                       CSBLNP            +  1
181100950309     O                       CSBNRS            +  1
181200950309     O                       CSBNSP            +  1
181300000404     O                                         +  1 'BOLLA NON CANC:TNCSB'
181400000404     O*------------------------------------
181500040715     O*****     E            NOCDCT         1
181600040715     O*****                  DCTAAS            +  5
181700040715     O*****                  DCTLNP            +  1
181800040715     O*****                  DCTNRS            +  1
181900040715     O*****                  DCTNSP            +  1
182000040715     O*****                                    +  1 'BOLLA NON CANC:FNDCT'
182100040715     O          E            NOCDCT         1
182200040715     O                       i12AAS            +  5
182300040715     O                       i12LNP            +  1
182400040715     O                       i12NRS            +  1
182500040715     O                       i12NSP            +  1
182600040715     O                                         +  1 'BOLLA NON CANC:FNDCT'
182700000404     O*------------------------------------
182800000404     O          E            NOCGCP         1
182900000404     O                       GCPAAS            +  5
183000000404     O                       GCPLNP            +  1
183100000404     O                       GCPNRS            +  1
183200000404     O                       GCPNSP            +  1
183300050301     O                                         +  1 'BOLLA NON CANC:TIGCP'
183400040922     O*------------------------------------
183500040922     O          E            ERRblp         1
183600040922     O                       blpAAS            +  5
183700040922     O                       blpLNP            +  1
183800040922     O                       blpNRS            +  1
183900040922     O                       blpNSP            +  1
184000040922     O                                         +  1 'ERR. Trovato reK su FNBLP'
184100040922     O*------------------------------------
184200040922     O          E            ERRarb         1
184300040922     O                       arbAAS            +  5
184400040922     O                       arbLNP            +  1
184500040922     O                       arbNRS            +  1
184600040922     O                       arbNSP            +  1
184700040922     O                                         +  1 'ERR. Trovato reK su FNARB'
184800061122     O          E            ERRSTO         1
184900061122     O                       TASAAS            +  5
185000061122     O                       TASLNP            +  1
185100061122     O                       TASNRS            +  1
185200061122     O                       TASNSP            +  1
185300061122     O                       TASTBL            +  1
185400061122     O                                         +  1 'ERR. WRITE STORICO '
185500040922     O*------------------------------------
185600950308     O          E            STAFIN         1
185700990802     O                                           30 'N.REC.TRASF. TITAS'
185800950308     O                       NUMTAS        2     44
185900950308     O          E            STAFIN         1
186000990802     O                                           30 'N.REC.TRASF. TITA7'
186100950308     O                       NUMTA7        2     44
186200990802     O          E            STAFIN         1
186300990802     O                                           30 'N.REC.TRASF. TITAA'
186400990802     O                       NUMTAA        2     44
186500990802     O          E            STAFIN         1
186600990802     O                                           30 'N.REC.TRASF. TITAT'
186700990802     O                       NUMTAT        2     44
186800990802     O          E            STAFIN         1
186900990802     O                                           30 'N.REC.TRASF. TITA4'
187000990802     O                       NUMTA4        2     44
187100010903     O          E            STAFIN         1
187200010903     O                                           30 'N.REC.TRASF. TITAH'
187300010903     O                       NUMTAH        2     44
187400950308     O          E            STAFIN         1
187500950308     O                                           30 'N.REC.TRASF. ECCET'
187600950308     O                       NUMCET        2     44
187700950308     O          E            STAFIN         1
187800950308     O                                           30 'N.REC.TRASF. ECCED'
187900950308     O                       NUMCED        2     44
188000980325     O          E            STAFIN         1
188100980325     O                                           30 'N.REC.TRASF. ECFIM'
188200980325     O                       NUMFIM        2     44
188300970903     O          E            STAFIN         1
188400970903     O                                           30 'N.REC.TRASF. ECECT'
188500970903     O                       NUMECT        2     44
188600970903     O          E            STAFIN         1
188700970903     O                                           30 'N.REC.TRASF. ECECD'
188800970903     O                       NUMECD        2     44
188900000327     O          E            STAFIN         1
189000000327     O                                           30 'N.REC.TRASF. FNEVB'
189100000327     O                       NUMEVB        2     44
189200000404     O          E            STAFIN         1
189300050301     O                                           30 'N.REC.TRASF. TIGCP'
189400000404     O                       NUMGCP        2     44
189500000404     O          E            STAFIN         1
189600050301     O                                           30 'N.REC.TRASF. TIGAP'
189700000404     O                       NUMGAP        2     44
189800000404     O          E            STAFIN         1
189900050301     O                                           30 'N.REC.TRASF. TIGNP'
190000000404     O                       NUMGNP        2     44
190100040310     O          E            STAFIN         1
190200040310     O                                           30 'N.REC. BOLLE FIAR5'
190300040310     O                       NUMAR5        2     44
190400060919     O          E            STAFIN         1
190500060919     O                                           30 'N.REC. BOLLE TITAI'
190600060919     O                       NUMTAI        2     44
190700081103     O          E            STAFIN         1
190800081103     O                                           30 'N.REC. BOLLE FIRAB'
190900081103     O                       NUMRAB        2     44
191000061122     O          E            STAFIN         1
191100061122     O                                           30 'N.BOLLE IN STORICO'
191200061122     O                       NUMSTO        2     44
191300001019     O          E            STAFIN         1
191400001020     O                                           30 'N.REC. BOLLE SCART'
191500001019     O                       NUMNON        2     44
191600021021     O          E            STAFIN         1
191700021021     O                                           30 'DI CUI:'
191800021021     O          E            STAFIN         1
191900021021     O                                           30 'X GIACENZA APERTA '
192000021021     O                       CNTGCP        2     44
192100021021     O          E            STAFIN         1
192200021021     O                                           30 'X DANNO PENDENTE  '
192300021021     O                       CNTDCT        2     44
192400021021     O          E            STAFIN         1
192500021021     O                                           30 'X C/A ATTIVO      '
192600021021     O                       CNTCSB        2     44
192700040922     O          E            STAFIN         1
192800040922     O                                           30 'N.REK.Trovati FNBLP'
192900040922     O                       NUMblp        2     44
193000040922     O          E            STAFIN         1
193100040922     O                                           30 'N.REK.Trovati FNARB'
193200040922     O                       NUMarb        2     44
193300060626     O          E            STAFIN         1
193400060626     O                                           +0 'Chiusuara pgm: FINTAS'
193500060626     O                       fintas              +1
193600060626     O                       tasdsp              +1
193700060626     O                       dsdatf              +1
193800060626     O               98                          +1 'SHTDN'
193900060626     O                                           +1 'FINL00'
194000060626     O                       finl00              +1
194100950308     O          E            STAFIN           60
194200931115     O                                           76 '*** FINE STAMPA ***'
194300931115     O                                          150 'Data:'
194400950717     O                       DSDAT6             159 '  /  /  '
194500990802     O                                          167 'Ora:'
194600950717     O                       DTORA6             178 '  :  :  '
194700940912     O*------------------------------------------------------------------------*
194800970903     O* AGGIORNAMENTO FILES
194900970903     O*------------------------------------------------------------------------*
195000970903     OECECT     E            AGGECT
195100970903     O                       ECTATB
195200970903     O                       ECTFTR
195300970903     OECECD     E            AGGECD
195400970903     O                       ECDATB
195500970903     O                       ECDFTR
195600970903     O*------------------------------------------------------------------------*
195700040823** cmd
195800040823OVRDBF FILE(        ) TOFILE(
195900931105**  TES - RIGHE IN STAMPA
196000931105------------------------------------------------------------------  1
196100931105 DATA    ORA       AA LNP SE N.SPED. TB     ERR  DESCRIZIONE ERROR  2
196200950308E                                                                   3
196300931105                                                                    4
196400931105                                                                    5
196500931105                                                                    6
196600931105                                                                    7
196700931105                                                                    8
196800931105                                                                    9
196900931105                                                                   10
197000931105                                                                   11
197100931105                                                                   12
197200931105                                                                   13
197300931105                                                                   14
197400931105                                                                   15
