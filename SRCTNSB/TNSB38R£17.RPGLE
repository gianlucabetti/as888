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
007900131029     FFNEVB22L  UF   E           K DISK
008000000404     F*---
008100000404     F* GIACENZE
008200000404     F*---
008300050301     Ftigcp51l  UF   E           K DISK
008400050301     Ftigcp00f  O  A E           K DISK
008500050301     F                                     RENAME(tigcp000:tigcpS)
008600000404     F*---
008700000404     F* GIACENZE -ANAGRAFICHE MITTENTE-
008800000404     F*---
008900050301     Ftigap01l  UF   E           K DISK
009000050301     Ftigap00f  O  A E           K DISK
009100050301     F                                     RENAME(TIGAP000:TIGAPS)
009200000404     F*---
009300000404     F* GIACENZE -NOTE-
009400000404     F*---
009500050301     FTIGNP01L  UF   E           K DISK
009600050301     FTIGNP00F  O  A E           K DISK
009700050301     F                                     RENAME(TIGNP000:TIGNPS)
009800040303     F*---
009900040303     F* ESTENSIONE ALLA TESTATA BOLLE
010000040303     F*---
010100040531     FFIAR531C  UF   E           K DISK
010200040303     FFIAR5P0F  O  A E           K DISK
010300040303     F                                     RENAME(FIAR5P00:FIAR5S)
010400060919     F*---
010500060919     Ftitai01l  UF   E           K DISK
010600060919     Ftitai00F  O  A E           K DISK
010700060919     F                                     RENAME(titai000:titaiS)
010800081103     F*---
010900081103     Ffirab01l  UF   E           K DISK
011000081103     Ffirab00F  O  A E           K DISK
011100081103     F                                     RENAME(firab000:firabS)
011200061122     F*---
011300061122     Ffiapd01l  IF   E           K DISK
011400080312     Ftilad01l  IF   E           K DISK
011500000404     F*---
011600000327     F* TABELLE
011700000327     F*---
011800950309     FTABEL00F  UF   E           K DISK    USROPN
011900000327     F*---
012000000327     F* CONTRASSEGNI
012100000327     F*---
012200950309     FTNCSB03L  IF   E           K DISK
012300000327     F*---
012400000327     F* DANNI
012500000327     F*---
012600040715     F***FNDCT02L  IF   E           K DISK
012700040823     F*---
012800040823     Ffnblp01L  IF   E           K DISK    usropn
012900040823     Ffnarb01L  IF   E           K DISK    usropn
013000000327     F*---
013100000327     F* PRINTER FILE
013200000327     F*---
013300931105     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
013400931105     F                                     FORMLEN(66)
013500931105     F                                     FORMOFL(60)
013600940912     L*------------------------------------------------------------------------*
013700040823     d wlib            s             10    inz('FILTRA201 ')
013800040910     d wlibp           s             10    inz('FILTRAPRD ')
013900040823     d comman          s             80
014000040823     d lenght          s             15  5 inz(80)
014100061122     d trcapd          s                   like(apdtip) inz('A')
014200061122     d com04           s              4  0
014300150123     d PARDTF_1m       s                   like(PARDTF)
014400150123     d Data_ISO        s               d   datfmt(*iso)
014500131223      *------------------------------------------------------------------------*
014600131223
014700131223     D*------------------
014800131223     D* VARIABILI D WRK
014900131223     D*------------------
015000131223     D  wAAS           S                   like(tasAAS) inz
015100131223     D  wLNP           S                   like(tasLNP) inz
015200131223     D  wNRS           S                   like(tasNRS) inz
015300131223     D  wNSP           S                   like(tasNSP) inz
015400131223
015500131223     D*------------------
015600131223     D* LINKING A DEFINIZIONI ESTERNE
015700131223     D*------------------
015800131223
015900131223     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
016000131223     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
016100131223
016200940912     D*------------------------------------------------------------------------*
016300940912     D* SCHIERE
016400940912     D*------------------------------------------------------------------------*
016500040823     d cmd             s             60    dim(01) ctdata perrcd(1)
016600931105     D TES             S             66    DIM(15) CTDATA PERRCD(1)
016700080925     d tbl             s              2    dim(50) inz
016800950309     D* DS TABELLA $3 (DATE AVANZAMENTO TRASFERIMENTO BOLLE)
016900950309     D DS$3          E DS
017000080925     D DStb          E DS
017100950717     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
017200950717     D DS§DAT          DS
017300950717     D  DSSEC                  1      2  0
017400950717     D  DSANN                  3      4  0
017500950717     D  DSMES                  5      6  0
017600950717     D  DSGIO                  7      8  0
017700950717     D  DSAAS                  1      4  0
017800950717     D  DSMGS                  5      8  0
017900950717     D  DSDAT6                 3      8  0
018000950717     D  DSDAT8                 1      8  0
018100950717     D DS§TIM          DS
018200950717     D  DTORA                  1      2  0
018300950717     D  DTMIN                  3      4  0
018400950717     D  DTSCN                  5      6  0
018500950717     D  DTGIO                  7      8  0
018600950717     D  DTMES                  9     10  0
018700950717     D  DTSEC                 11     12  0
018800950717     D  DTANN                 13     14  0
018900950717     D  DTORA6                 1      6  0
019000950717     D  DTTIME                 1     14  0
019100940912     D*------------------------------------------------------------------------*
019200940912     D* INPUT
019300940912     D*------------------------------------------------------------------------*
019400941123     D                 DS
019500941123     D  DSAASI                 1      4  0
019600941123     D  DSMGSI                 5      8  0
019700950309     D  DSDATI                 1      8  0
019800941123     D                 DS
019900941123     D  DSAASF                 1      4  0
020000941123     D  DSMGSF                 5      8  0
020100950309     D  DSDATF                 1      8  0
020200020220     D                 DS
020300020220     D  DSAASFP                1      4  0
020400020220     D  DSMGSFP                5      8  0
020500020220     D  DSDATFP                1      8  0
020600960413     D*-------------------
020700960413     D* DATA SPEDIZIONE
020800960413     D*-------------------
020900960413     D                 DS
021000960413     D  TASAAS                 1      4  0
021100960413     D  TASMGS                 5      8  0
021200960413     D  TASDSP                 1      8  0
021300150123     D  TASANME                1      6  0
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
023700150123     D  PARDTFANME             9     14  0                                      DT.FIN AAAAMM
023800020220     D  PARDFP                17     24  0                                      DT.FIN PT
023900050706     D  PARcop                25     27                                         DT.FIN PT
024000940912     D*-------------------
024100940912     D* ARCHITETTURA
024200940912     D*-------------------
024300931115     D KPJBA         E DS
024400050705     D tnsb34ds      E DS
024500050705     D dtasflo       E DS
024600061121      *
024700061121     D*-------------------
024800061121     D* ANAGRAFICA
024900061121     D*-------------------
025000061121     d TIBS69ds      e ds                  inz
025100061121     d DS_cnaco      e ds                  inz  extname(CNACO00F)
025200061121     d DS_cnind      e ds                  inz  extname(CNIND00F)
025300061121     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
025400061121     d DS_fncls      e ds                  inz  extname(FNCLS00F)
025500061122     D*-------------------
025600061122     D* RIFERIMENTI ALFANUMERICI
025700061122     D*-------------------
025800061122     d DTA4A         e ds
025900061122     D*-------------------
026000061122     D* CODICE FISCALE
026100061122     D*-------------------
026200061122     d DTA4Q         e ds
026300120709
026400120709      * RCD DOC FIAR5
026500120709     d dAR5doc       e ds
026600160927      * RCD FAT FIAR5
026700160927     d dAR5fat       e ds
026800120709
026900940912     C*------------------------------------------------------------------------*
027000940912     C*?MAIN LINE
027100940912     C*------------------------------------------------------------------------*
027200000000     C*
027300930129     C* ROUTINE INIZIALE
027400930129     C                   EXSR      RUTINZ
027500931105     C*
027600931105     C* STAMPA RIGA DI TESTATA
027700931105     C                   EXCEPT    TESTA
027800930601     C*
027900930601     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
028000930601     C     FINL00        IFEQ      '0'                                          --- 1 -->
028100000000     C*
028200990802     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA SU TITAS
028300950307     C                   EXSR      SETTAS
028400930128     C*
028500990802     C* CICLO FINO A FINE FILE TITAS OPPURE
028600950308     C* RICHIESTA CHIUSURA PGM OPPURE
028700950308     C* FINE PGM PER ERRORE
028800950308     C                   SETOFF                                       98
028900950307     C     FINTAS        DOWEQ     '0'                                          --- 2 -->
029000950308     C     *IN98         ANDEQ     '0'
029100950308     C     FINL00        ANDEQ     '0'
029200080214      * verifico   che non esistano ARB e BLP altrimenti non cancello
029300040823      * e leggo un'altro rekord
029400040823     C     Kbol          Chain     fnblp01l                           33
029500040922     C  N33              ADD       1             NUMblp                         *REC TRASF TITA7
029600040922     C  n33              EXCEPT    ERRblp
029700080214     C     kbol          Chain     fnarb01l                           34
029800080214     C  N34              ADD       1             NUMarb                         *REC TRASF TITA7
029900080214     C  n34              EXCEPT    ERRarb
030000080214     c                   if        not *in33 or not *in34
030100080214     c                   goto      altrorek
030200080214     c                   end
030300950307     C*
030400001019     C                   WRITE     TITASS00                             99      *SCRIVO TITASS
030500001019     C  N99              DELETE    TITASP00                             99      *CANCELLO TITASP
030600950307     C   99              EXCEPT    ERRTAS
030700950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
030800990802     C  N99              ADD       1             NUMTAS                         *REC TRASF TITAS
030900050705     c* se bolla ha immagine di laguna scrivo record sul file TILAD00F
031000050705     c                   if        not *in99
031100050705     c                   movel     tasflo        dtasflo
031200050705     c                   if        §floiml <> ' '
031300050705     c                   exsr      srlaguna
031400050705     c                   end
031500120709       //?se la bolla ha le immagini dei documenti firmati dal cliente da
031600120709       //?tornare al mittente scrivo nuovo record sul file TIJDD00F
031700120709      /free
031800120709        AR5trd = 'DOC';
031900120709        chain (TASaas:TASlnp:TASnrs:TASnsp:AR5trd) FIAR531C;
032000120709        IF  %found(FIAR531C);
032100120709          dAR5doc = AR5uni;
032200120709          IF  §AR5jfid <> *blanks;
032300120709            exsr srJdoc;
032400120709          ENDIF;
032500120709        ENDIF;
032600120709      /end-free
032700050705     c                   end
032800950307     C*
032900990802     C* SE ESISTE TITA7 TRASFERISCO ANCHE QUELLO
033000950307     C     TASSV3        IFNE      *BLANKS                                      --- 3 -->
033100990802     C                   EXSR      SETTA7                                       *PRIMO TITA7
033200950307     C     FINTA7        DOWEQ     '0'                                          --- 4 -->
033300990802     C                   WRITE     TITA7S00                             99      *SCRIVO TITA7S
033400001019     C  N99              DELETE    TITA730C                             99      *CANCELLO TITA730C
033500950307     C   99              EXCEPT    ERRTA7
033600950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
033700990802     C  N99              ADD       1             NUMTA7                         *REC TRASF TITA7
033800990802     C                   EXSR      LETTA7                                       *SUCCESSIVO TITA7
033900950307     C                   ENDDO                                                  <-- 4 ---
034000950307     C                   ENDIF                                                  <-- 3 ---
034100990802     C*
034200990802     C* TRASFERISCE TITAA - ANAGRAFICHE
034300990802     C                   EXSR      SETTAA                                       *PRIMO TITAA
034400990802DO  3C     FINTAA        DOWEQ     '0'
034500990802     C                   WRITE     TITAAS00                             99      *SCRIVO TITAAS
034600001019     C  N99              DELETE    TITAA30C                             99      *CANCELLO TITAA30C
034700990802     C   99              EXCEPT    ERRTAA
034800990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
034900990802     C  N99              ADD       1             NUMTAA                         *REC TRASF TITAA
035000990802     C                   EXSR      LETTAA                                       *SUCCESSIVO TITAA
035100990802E   3C                   ENDDO
035200061121     C* verifico se scritto i dati anagrafici del mittente altrimenti aggancio anagrafica
035300061121     c                   if        tasrsm = *blanks
035400061121     c                   exsr      recana
035500061121     c                   endif
035600990802     C*
035700990802     C* TRASFERISCE TITAT - SEGNACOLLI
035800990802     C                   EXSR      SETTAT                                       *PRIMO TITAT
035900990802DO  3C     FINTAT        DOWEQ     '0'
036000990802     C                   WRITE     TITATS00                             99      *SCRIVO TITATS
036100001019     C  N99              DELETE    TITAT30C                             99      *CANCELLO TITAT30C
036200990802     C   99              EXCEPT    ERRTAT
036300990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
036400990802     C  N99              ADD       1             NUMTAT                         *REC TRASF TITAT
036500990802     C                   EXSR      LETTAT                                       *SUCCESSIVO TITAT
036600990802E   3C                   ENDDO
036700990802     C*
036800990802     C* TRASFERISCE TITA4 - RIFERIMENTI
036900990802     C                   EXSR      SETTA4                                       *PRIMO TITA4
037000990802DO  3C     FINTA4        DOWEQ     '0'
037100990802     C                   WRITE     TITA4S00                             99      *SCRIVO TITA4S
037200001019     C  N99              DELETE    TITA430C                             99      *CANCELLO TITA430C
037300990802     C   99              EXCEPT    ERRTA4
037400990802     C   99              MOVEL     '1'           FINL00                         *FINE PGM
037500990802     C  N99              ADD       1             NUMTA4                         *REC TRASF TITA4
037600990802     C                   EXSR      LETTA4                                       *SUCCESSIVO TITA4
037700990802E   3C                   ENDDO
037800010903     C*
037900010903     C* TRASFERISCE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
038000010903     C                   EXSR      SETTAH                                       *PRIMO TITAH
038100010903DO  3C     FINTAH        DOWEQ     '0'
038200010903     C                   WRITE     TITAHS00                             99      *SCRIVO TITAHS
038300010903     C  N99              DELETE    TITAH30C                             99      *CANCELLO TITAH30C
038400010903     C   99              EXCEPT    ERRTAH
038500010903     C   99              MOVEL     '1'           FINL00                         *FINE PGM
038600010903     C  N99              ADD       1             NUMTAH                         *REC TRASF TITAH
038700010903     C                   EXSR      LETTAH                                       *SUCCESSIVO TITAH
038800010903E   3C                   ENDDO
038900950307     C*
039000950307     C* SE ESISTE ECCET LO TRASFERISCE
039100971008     C                   Z-ADD     999           MINNBC                         MIMIMO BLOCCO
039200950307     C                   Z-ADD     TASAAS        CETAAS                         ANNO SPEDIZIONE
039300950307     C                   Z-ADD     TASLNP        CETLNP                         LINEA DI PARTENZA
039400950307     C                   Z-ADD     TASNRS        CETNRS                         NUMERO SERIE
039500950307     C                   Z-ADD     TASNSP        CETNSP                         NUMERO SPEDIZIONE
039600950307     C                   MOVEL     TASTBL        CETTBL                         TIPO BOLLA
039700950307     C     KEYCET        CHAIN     ECCET30C                           99
039800950307     C     *IN99         IFEQ      *OFF                                         --- 3 --> *TROVATO
039900970903     C*
040000970903     C* MEMORIZZA IL NUMERO BLOCCO COMPETENZE TESTATA PIU' PICCOLO
040100971008IF  4C     CETNBC        IFGT      *ZEROS
040200971008IF  5C     CETNBC        IFLT      MINNBC
040300970903     C                   Z-ADD     CETNBC        MINNBC
040400971008E   5C                   ENDIF
040500971008E   4C                   ENDIF
040600970903     C*
040700950309     C                   WRITE     ECCETS                               99      *SCRIVO ECCETS
040800001019     C  N99              DELETE    ECCET30C                             99      *CANCELLO ECCET30C
040900950307     C   99              EXCEPT    ERRCET
041000950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
041100950308     C  N99              ADD       1             NUMCET                         *REC TRASF ECCET
041200950307     C*
041300950307     C* SE ESISTE ECCED TRASFERISCO ANCHE QUELLO
041400950307     C     CETDET        IFEQ      'S'                                          --- 4 -->
041500950307     C                   EXSR      SETCED                                       *PRIMO ECCED
041600950307     C     FINCED        DOWEQ     '0'                                          --- 5 -->
041700950309     C                   WRITE     ECCEDS                               99      *SCRIVO ECCEDS
041800001020     C  N99              DELETE    ECCED30C                             99      *CANCELLO ECCED30C
041900950307     C   99              EXCEPT    ERRCED
042000950308     C   99              MOVEL     '1'           FINL00                         *FINE PGM
042100950308     C  N99              ADD       1             NUMCED                         *REC TRASF ECCED
042200950307     C                   EXSR      LETCED                                       *SUCCESSIVO ECCED
042300950307     C                   ENDDO                                                  <-- 5 ---
042400950307     C                   ENDIF                                                  <-- 4 ---
042500950307     C                   ENDIF                                                  <-- 3 ---
042600000327     C*
042700000327     C* SE ESISTE FNEVB LO TRASFERISCE
042800000327     C                   Z-ADD     TASAAS        EVBAAS                         ANNO SPEDIZIONE
042900000327     C                   Z-ADD     TASLNP        EVBLNP                         LINEA DI PARTENZA
043000000327     C                   Z-ADD     TASNRS        EVBNRS                         NUMERO SERIE
043100000327     C                   Z-ADD     TASNSP        EVBNSP                         NUMERO SPEDIZIONE
043200001019     C     KEYEVB        CHAIN     FNEVB01L                           99
043300000327     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
043400000327     C                   WRITE     FNEVBS                               99      *SCRIVE
043500001019     C  N99              DELETE    FNEVB01L                             99      *CANCELLA
043600131029     c                   if        not *in99
043700131029     C     Kevb2f        chain     FNEVB22L
043800131029     c                   if        %found(fnevb22l)
043900131029     c                   delete    fnevb22l
044000131029     c                   endif
044100131029     c                   endif
044200000327     C   99              EXCEPT    ERREVB
044300000327     C   99              MOVEL     '1'           FINL00                         *FINE PGM
044400000327     C  N99              ADD       1             NUMEVB                         *REC TRASF FNEVB
044500000327     C     KEYEVB        READE     FNEVB01L                               99
044600000327     C                   ENDDO                                                  <-- 3 ---
044700000404     C*
044800050301     C* SE ESISTE TIGCP LO TRASFERISCE
044900001018     C     TASFGC        IFEQ      'S'
045000000404     C                   Z-ADD     TASAAS        KCPAAS                         ANNO SPEDIZIONE
045100000404     C                   Z-ADD     TASLNP        KCPLNP                         LINEA DI PARTENZA
045200000404     C                   Z-ADD     TASNRS        KCPNRS                         NUMERO SERIE
045300000404     C                   Z-ADD     TASNSP        KCPNSP                         NUMERO SPEDIZIONE
045400050301     C     KE2GCP        CHAIN     TIGCP51L                           99
045500001018     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
045600050303     c     gcpsede       ifeq      'S'
045700050301     C                   WRITE     TIGCPS                               99      *SCRIVE
045800050301     C  N99              DELETE    TIGCP51L                             99      *CANCELLA
045900000404     C   99              EXCEPT    ERRGCP
046000000404     C   99              MOVEL     '1'           FINL00                         *FINE PGM
046100000404     C  N99              ADD       1             NUMGCP                         *REC TRASF FNEVB
046200000404     C*
046300050301     C* SE ESISTE TIGAP LO TRASFERISCE
046400000404     C                   Z-ADD     GCPAAS        KAPAAS                         ANNO SPEDIZIONE
046500000404     C                   Z-ADD     GCPLNP        KAPLNP                         LINEA DI PARTENZA
046600000404     C                   Z-ADD     GCPNRS        KAPNRS                         NUMERO SERIE
046700000404     C                   Z-ADD     GCPNSP        KAPNSP                         NUMERO SPEDIZIONE
046800050301     C     KEYGAP        CHAIN     TIGAP01L                           99
046900001018     C     *IN99         DOWEQ     *OFF
047000050301     C                   WRITE     TIGAPS                               99      *SCRIVE
047100050301     C  N99              DELETE    TIGAP01L                             99      *CANCELLA
047200001017     C   99              EXCEPT    ERRGAP
047300001017     C   99              MOVEL     '1'           FINL00                         *FINE PGM
047400001017     C  N99              ADD       1             NUMGAP                         *REC TRASF FNEVB
047500050301     C     KEYGAP        READE     TIGAP01L                               99
047600001018     C                   ENDDO
047700000404     C*
047800050301     C* SE ESISTE TIGNP LO TRASFERISCE
047900000404     C                   Z-ADD     GCPAGC        KNPAGC                         ANNO APERTURA
048000000404     C                   Z-ADD     GCPFGC        KNPFGC                         FILIALE APERTURA
048100000404     C                   Z-ADD     GCPNGC        KNPNGC                         NUMERO GIACENZA
048200050301     C     KEYGNP        CHAIN     TIGNP01L                           99
048300001018     C     *IN99         DOWEQ     *OFF
048400050301     C                   WRITE     TIGNPS                               99      *SCRIVE
048500050301     C  N99              DELETE    TIGNP01L                             99      *CANCELLA
048600001017     C   99              EXCEPT    ERRGNP
048700001017     C   99              MOVEL     '1'           FINL00                         *FINE PGM
048800001017     C  N99              ADD       1             NUMGNP                         *REC TRASF FNEVB
048900050301     C     KEYGNP        READE     TIGNP01L                               99
049000001018     C                   ENDDO
049100050303     c                   else
049200050303     C                   EXCEPT    ERRGCP1
049300060707     C*                  MOVEL     '1'           FINL00                         *FINE PGM
049400050303     C                   ENDIF
049500000404     C*
049600000404     C* LETTURA SUCCESSIVA GIACENZE
049700050301     C     KE2GCP        READE     TIGCP51L                               99
049800001018     C                   ENDDO
049900001018     C                   ENDIF
050000040303     C*
050100040303     C* SE ESISTE FIAR5 LO TRASFERISCE
050200040303     C                   Z-ADD     TASAAS        AR5AAS                         ANNO SPEDIZIONE
050300040303     C                   Z-ADD     TASLNP        AR5LNP                         LINEA DI PARTENZA
050400040303     C                   Z-ADD     TASNRS        AR5NRS                         NUMERO SERIE
050500040303     C                   Z-ADD     TASNSP        AR5NSP                         NUMERO SPEDIZIONE
050600160927     c                   clear                   dar5fat
050700040531     C     KEYAR5        CHAIN     FIAR531C                           99
050800040303     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
050900160927      *se trovo rekord FAT mi salvo i dati
051000160927     c                   if        ar5trd = 'FAT'
051100160927     c                   movel     ar5uni        dar5fat
051200160927     c                   endif
051300040303     C                   WRITE     FIAR5S                               99      *SCRIVE
051400040531     C  N99              DELETE    FIAR531C                             99      *CANCELLA
051500040303     C   99              EXCEPT    ERRAR5
051600040303     C   99              MOVEL     '1'           FINL00                         *FINE PGM
051700040303     C  N99              ADD       1             NUMAR5                         *REC TRASF FNEVB
051800040531     C     KEYAR5        READE     FIAR531C                               99
051900040303     C                   ENDDO                                                  <-- 3 ---
052000060919     C*
052100060919     C* SE ESISTE TITAI LO TRASFERISCE
052200060919     C                   Z-ADD     TASAAS        taiAAS                         ANNO SPEDIZIONE
052300060919     C                   Z-ADD     TASLNP        taiLNP                         LINEA DI PARTENZA
052400060919     C                   Z-ADD     TASNRS        taiNRS                         NUMERO SERIE
052500060919     C                   Z-ADD     TASNSP        taiNSP                         NUMERO SPEDIZIONE
052600060919     C                   movel     TASTBL        taiTBL                         NUMERO SPEDIZIONE
052700060919     C     KEYtai        CHAIN     titai01l                           99
052800060919     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
052900060919     C                   WRITE     titaiS                               99      *SCRIVE
053000060919     C  N99              DELETE    titai01l                             99      *CANCELLA
053100060919     C   99              EXCEPT    ERRtai
053200060919     C   99              MOVEL     '1'           FINL00                         *FINE PGM
053300060919     C  N99              ADD       1             NUMtai                         *REC TRASF FNEVB
053400060919     C     KEYtai        READE     titai01l                               99
053500060919     C                   ENDDO                                                  <-- 3 ---
053600081103     C* SE ESISTE FIRAB LO TRASFERISCE
053700081103     C                   Z-ADD     TASAAS        rabAAS                         ANNO SPEDIZIONE
053800081103     C                   Z-ADD     TASLNP        rabLNP                         LINEA DI PARTENZA
053900081103     C                   Z-ADD     TASNRS        rabNRS                         NUMERO SERIE
054000081103     C                   Z-ADD     TASNSP        rabNSP                         NUMERO SPEDIZIONE
054100081103     C     KEYrab        CHAIN     firab01l                           99
054200081103     C     *IN99         DOWEQ     *OFF                                         --- 3 --> *TROVATO
054300081103     C                   WRITE     firabS                               99      *SCRIVE
054400081103     C  N99              DELETE    firab01l                             99      *CANCELLA
054500081103     C   99              EXCEPT    ERRrab
054600081103     C   99              MOVEL     '1'           FINL00                         *FINE PGM
054700081103     C  N99              ADD       1             NUMrab                         *REC TRASF FNEVB
054800081103     C     KEYrab        READE     firab01l                               99
054900081103     C                   ENDDO                                                  <-- 3 ---
055000061214      * decodifico alcuni dati per il nuovo storico  e lo scrivo se non bolla Poste
055100061214     c                   If        tastsp <> 'P'
055200080926     c* se bolla di recupero non scrivo storico
055300080926     c                   z-add     1             conta
055400080926     c     tastbl        lookup    tbl(conta)                             44
055500080926     c                   if        not *in44
055600061122     c                   EXSR      wrttassto
055700061214     c                   endif
055800080926     c                   endif
055900061122
056000000000     C*
056100980325     C* LETTURA SUCCESSIVA BOLLE
056200040823     c     altrorek      tag
056300040823
056400060707     c                   if        finl00 = '0'
056500990802     C                   EXSR      LETTAS                                       *SUCCESSIVO TITAS
056600060707     c                   end
056700950308     C                   SHTDN                                        98        *CONTR.CHIUSURA PGM
056800930128     C*
056900950307     C                   ENDDO                                                  <-- 2 ---
057000950307     C                   ENDIF                                                  <-- 1 ---
057100970903     C*
057200970903     C* PULISCE I CODICI COMPETENZA -TESTATA E DETTAGLIO- CON NUMERO BLOCCO
057300970903     C* MINORE DI QUELLO PIU' PICCOLO CON CUI SONO STATE CALCOLATE LE BOLLE
057400971008IF  1C     MINNBC        IFNE      999
057500970903     C     *LOVAL        SETLL     ECECT01L
057600970903     C                   READ      ECECT01L                               97
057700971008DO  2C     *IN97         DOWEQ     *OFF
057800971008IF  3C     ECTATB        IFEQ      *BLANKS
057900971008IF  4C     ECTNBC        IFLT      MINNBC
058000970903     C                   MOVEL     'A'           ECTATB
058100970903     C                   MOVEL     *BLANKS       ECTFTR
058200970903     C                   EXCEPT    AGGECT                                       *AGGIORNAMENTO CAMPI
058300970903     C                   WRITE     ECECTS                               97      *SCRIVO ECECTS
058400970903     C   97              EXCEPT    ERRECT                                       *SCRIVE ERRORE
058500970903     C   97              MOVEL     '1'           FINL00                         *FINE PGM
058600970903     C  N97              ADD       1             NUMECT                         *REC TRASF ECECT
058700971008E   4C                   ENDIF
058800971008E   3C                   ENDIF
058900970903     C                   READ      ECECT01L                               97
059000971008E   2C                   ENDDO
059100950309     C*
059200970903     C     *LOVAL        SETLL     ECECD01L
059300970903     C                   READ      ECECD01L                               97
059400971008DO  2C     *IN97         DOWEQ     *OFF
059500971008IF  3C     ECDATB        IFEQ      *BLANKS
059600971008IF  4C     ECDNBC        IFLT      MINNBC
059700970903     C                   MOVEL     'A'           ECDATB
059800970903     C                   MOVEL     *BLANKS       ECDFTR
059900970903     C                   EXCEPT    AGGECD                                       *AGGIORNAMENTO CAMPI
060000970903     C                   WRITE     ECECDS                               97      *SCRIVO ECECDS
060100970903     C   97              EXCEPT    ERRECD                                       *SCRIVE ERRORE
060200970903     C   97              MOVEL     '1'           FINL00                         *FINE PGM
060300970903     C  N97              ADD       1             NUMECD                         *REC TRASF ECECD
060400971008E   4C                   ENDIF
060500971008E   3C                   ENDIF
060600970903     C                   READ      ECECD01L                               97
060700971008E   2C                   ENDDO
060800971008E   1C                   ENDIF
060900970903     C*
061000950309     C* AGGIORNA LA TABELLA CON L'ULTIMA DATA DI TRASFERIM. (SOLO SE TUTTO OK)
061100950309     C     *IN98         IFEQ      '0'
061200950309     C     FINL00        ANDEQ     '0'
061300950309     C                   EXSR      AGGTAB
061400950309     C                   ENDIF
061500931105     C*
061600931105     C* STAMPA RIGA DI FINE STAMPA
061700950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
061800931105     C                   EXCEPT    STAFIN
061900050705     c* chiudo il pgm di archiviazione Laguna
062000050705     C                   CLEAR                   Tnsb34ds
062100050705     C                   MOVEL     'C'           sb34tla
062200050705     C                   CALL      'TNSB34R'
062300050705     C                   PARM                    TNSB34ds
062400000000     C*
062500000000     C                   SETON                                        LR
062600940912     C*------------------------------------------------------------------------*
062700990802     C*? SETTAS - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAS
062800940912     C*------------------------------------------------------------------------*
062900950307     C     SETTAS        BEGSR
063000931115     C*
063100931115     C* AZZERO CONTATORE RECORD LETTI
063200950307     C                   MOVEL     '0'           FINTAS            1
063300930208     C*
063400930316     C* POSIZIONAMENTO E PRIMA LETTURA
063500941123     C                   Z-ADD     PARDTI        DSDATI
063600941123     C                   Z-ADD     PARDTF        DSDATF
063700020220     C                   Z-ADD     PARDFP        DSDATFP
063800941123     C                   Z-ADD     DSAASI        TASAAS
063900941123     C                   Z-ADD     DSMGSI        TASMGS
064000990802     C     KEYTAS        SETLL     TITAS38C                           99        *FINE-ARCHIV
064100930625     C*
064200930625     C     *IN99         IFEQ      '1'
064300950307     C                   MOVEL     '1'           FINTAS
064400930625     C                   ELSE
064500950307     C                   EXSR      LETTAS
064600001020     C                   ENDIF
064700930208     C*
064800930208     C                   ENDSR
064900940912     C*------------------------------------------------------------------------*
065000990802     C*? LETTAS - LETTURA PROSSIMO RECORD TITAS
065100940912     C*------------------------------------------------------------------------*
065200950307     C     LETTAS        BEGSR
065300930625     C*
065400930625     C* LEGGO FINO A:
065500930625     C*     - FINE FILE O SUPERATO LIMITE FINALE
065600930625     C*     - TROVATO RECORD VALIDO
065700930625     C                   MOVEL     'N'           WRECOK
065800950307     C     FINTAS        DOWEQ     '0'                                          --- 1 -->
065900930625     C     WRECOK        ANDEQ     'N'
066000061121      * pulisco lo storico
066100061121     c                   clear                   titassto
066200930208     C*
066300930316     C* LETTURE SUCCESSIVE ALLA PRIMA
066400990802     C                   READ      TITAS38C                               99
066500960413     C*
066600960413     C     *IN99         IFEQ      '1'                                          *FINE FILE
066700020220     C     TASDSP        ORGT      DSDATF                                       *SUPERATA DATA FINE
066800950307     C                   MOVEL     '1'           FINTAS
066900930625     C                   ELSE
067000950309     C                   EXSR      CHKTAS                                       *CONTR.VALIDITA REC.
067100001019     C                   ENDIF
067200930625     C*
067300001019     C                   ENDDO                                                  <-- 1 ---
067400930208     C*
067500930208     C                   ENDSR
067600950307     C*------------------------------------------------------------------------*
067700990802     C*? SETTA7 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA7
067800950307     C*------------------------------------------------------------------------*
067900950307     C     SETTA7        BEGSR
068000950307     C*
068100950307     C* AZZERO CONTATORE RECORD LETTI
068200950307     C                   MOVEL     '0'           FINTA7            1
068300950307     C*
068400950307     C* POSIZIONAMENTO E PRIMA LETTURA
068500950307     C                   Z-ADD     TASAAS        TA7AAS
068600950307     C                   Z-ADD     TASLNP        TA7LNP
068700950307     C                   Z-ADD     TASNRS        TA7NRS
068800950307     C                   Z-ADD     TASNSP        TA7NSP
068900950307     C                   MOVEL     TASTBL        TA7TBL
069000001019     C*
069100001019     C     KEYTA7        CHAIN     TITA730C                           99        *FINE-ARCHIV
069200950307     C     *IN99         IFEQ      '1'
069300950307     C                   MOVEL     '1'           FINTA7
069400001019     C                   ENDIF
069500950307     C*
069600950307     C                   ENDSR
069700950307     C*------------------------------------------------------------------------*
069800990802     C*? LETTA7 - LETTURA PROSSIMO RECORD TITA7
069900950307     C*------------------------------------------------------------------------*
070000001019     C     LETTA7        BEGSR
070100001020     C*
070200001020     C* LETTURE SUCCESSIVE ALLA PRIMA
070300010124     C                   READ      TITA730C                               99
070400950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
070500010124     C     TA7AAS        ORNE      TASAAS
070600010124     C     TA7LNP        ORNE      TASLNP
070700010124     C     TA7NRS        ORNE      TASNRS
070800010124     C     TA7NSP        ORNE      TASNSP
070900010124     C     TA7TBL        ORNE      TASTBL
071000950307     C                   MOVEL     '1'           FINTA7
071100001019     C                   ENDIF
071200950307     C*
071300950307     C                   ENDSR
071400990802     C*------------------------------------------------------------------------*
071500990802     C*? SETTAA - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAA - ANAGRAFICHE
071600990802     C*------------------------------------------------------------------------*
071700990802     C     SETTAA        BEGSR
071800990802     C*
071900990802     C* AZZERO CONTATORE RECORD LETTI
072000990802     C                   MOVEL     '0'           FINTAA            1
072100990802     C*
072200990802     C* POSIZIONAMENTO E PRIMA LETTURA
072300990802     C                   Z-ADD     TASAAS        TAAAAS
072400990802     C                   Z-ADD     TASLNP        TAALNP
072500990802     C                   Z-ADD     TASNRS        TAANRS
072600990802     C                   Z-ADD     TASNSP        TAANSP
072700001019     C     KEYTAA        CHAIN     TITAA30C                           99        *FINE-ARCHIV
072800990802     C     *IN99         IFEQ      '1'
072900990802     C                   MOVEL     '1'           FINTAA
073000061121      * recupero dati anagrafici del mittente
073100061121     c                   else
073200061121     c                   exsr      rectaa
073300001019     C                   ENDIF
073400990802     C*
073500990802     C                   ENDSR
073600990802     C*------------------------------------------------------------------------*
073700990802     C*? LETTAA - LETTURA PROSSIMO RECORD TITAA - ANAGRAFICHE
073800990802     C*------------------------------------------------------------------------*
073900990802     C     LETTAA        BEGSR
074000010124     C*
074100010124     C                   READ      TITAA30C                               99
074200990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
074300010130     C     TAAAAS        ORNE      TASAAS
074400010130     C     TAALNP        ORNE      TASLNP
074500010130     C     TAANRS        ORNE      TASNRS
074600010130     C     TAANSP        ORNE      TASNSP
074700990802     C                   MOVEL     '1'           FINTAA
074800001019     C                   ENDIF
074900061121      * se letto
075000061123     c                   if        fintaa = '0'
075100061121     c                   exsr      rectaa
075200061121     c                   endif
075300990802     C*
075400990802     C                   ENDSR
075500061121     C*------------------------------------------------------------------------*
075600061121     C*? RECTAA - RECUPERO DATI ANAGRAFICI MITTENTE
075700061121     C*------------------------------------------------------------------------*
075800061121     C     RECTAA        BEGSR
075900061121      * dati anagrafici mittente
076000061121     c                   IF        taatrc = 'M'
076100061121     c                   eval      tasrsm = taarsc
076200061121     c                   eval      tasinm = taaind
076300061121     c                   eval      tascam = taacap
076400061121     c                   eval      taslom = taaloc
076500061121     c                   eval      tasprm = taaprv
076600061121     c                   eval      tasnzm = taanaz
076700061122      * iva  per i franchi non coidficati
076800061122     c                   if        %subst(tastbl:1:1) = 'F'
076900061122     c                   eval      tasiva = taacpi
077000061122     c                   endif
077100061122
077200061121     c                   endif
077300061121
077400061121     c                   endsr
077500061121     C*------------------------------------------------------------------------*
077600061121     C*? RECANA - RECUPERO DA ANAGRAFICO CLIENTE
077700061121     C*------------------------------------------------------------------------*
077800061121     C     RECANA        BEGSR
077900061121     C                   clear                   ds_CNACO
078000061121     C                   clear                   ds_CNIND
078100061121     C                   clear                   ds_CNCLP
078200061121     C                   clear                   ds_FNCLS
078300061121     c                   z-add     TASccm        I69kac
078400061121     c                   z-add     TASccm        I69kin
078500061121     c                   call      'TIBS69R'
078600061121     c                   parm                    TIBS69ds
078700061121     c                   parm                    ds_CNACO
078800061121     c                   parm                    ds_CNIND
078900061121     c                   parm                    ds_CNCLP
079000061121     c                   parm                    ds_FNCLS
079100061122if  3c                   if        O69err <> *on
079200061122     c                   movel     ACOrag        TASrsm
079300061122     c                   movel     INDvia        TASinm
079400061122     c                   movel     INDcap        TAScam
079500061122     c                   movel     INDcit        TASlom
079600061122     c                   movel     INDprv        TASprm
079700061122     c                   movel     INDsta        TASnzm
079800061122e   3c                   endif
079900061121
080000061121     c                   endsr
080100061122     C*------------------------------------------------------------------------*
080200061122     C*? RECIVA - RECUPERO DA ANAGRAFICO CLIENTE IVA E/O CODICE FISCALE
080300061122     C*------------------------------------------------------------------------*
080400061122     C     RECIVA        BEGSR
080500061122     C                   clear                   ds_CNACO
080600061122     C                   clear                   ds_CNIND
080700061122     C                   clear                   ds_CNCLP
080800061122     C                   clear                   ds_FNCLS
080900061122     c                   z-add     TASKSC        I69kin
081000061122     c                   call      'TIBS69R'
081100061122     c                   parm                    TIBS69ds
081200061122     c                   parm                    ds_CNACO
081300061122     c                   parm                    ds_CNIND
081400061122     c                   parm                    ds_CNCLP
081500061122     c                   parm                    ds_FNCLS
081600061122
081700061122     c                   endsr
081800990802     C*------------------------------------------------------------------------*
081900990802     C*? SETTAT - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAT - SEGNACOLLI
082000990802     C*------------------------------------------------------------------------*
082100990802     C     SETTAT        BEGSR
082200990802     C*
082300990802     C* AZZERO CONTATORE RECORD LETTI
082400990802     C                   MOVEL     '0'           FINTAT            1
082500990802     C*
082600990802     C* POSIZIONAMENTO E PRIMA LETTURA
082700990802     C                   Z-ADD     TASAAS        TATAAS
082800990802     C                   Z-ADD     TASLNP        TATLNP
082900990802     C                   Z-ADD     TASNRS        TATNRS
083000990802     C                   Z-ADD     TASNSP        TATNSP
083100001019     C     KEYTAT        CHAIN     TITAT30C                           99        *FINE-ARCHIV
083200990802     C     *IN99         IFEQ      '1'
083300990802     C                   MOVEL     '1'           FINTAT
083400001019     C                   ENDIF
083500990802     C*
083600990802     C                   ENDSR
083700990802     C*------------------------------------------------------------------------*
083800990802     C*? LETTAT - LETTURA PROSSIMO RECORD TITAT - SEGNACOLLI
083900990802     C*------------------------------------------------------------------------*
084000990802     C     LETTAT        BEGSR
084100010124     C*
084200010124     C                   READ      TITAT30C                               99
084300990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
084400010124     C     TATAAS        ORNE      TASAAS
084500010124     C     TATLNP        ORNE      TASLNP
084600010124     C     TATNRS        ORNE      TASNRS
084700010124     C     TATNSP        ORNE      TASNSP
084800990802     C                   MOVEL     '1'           FINTAT
084900001019     C                   ENDIF
085000990802     C*
085100990802     C                   ENDSR
085200990802     C*------------------------------------------------------------------------*
085300990802     C*? SETTA4 - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITA4 - RIFERIMENTI
085400990802     C*------------------------------------------------------------------------*
085500990802     C     SETTA4        BEGSR
085600990802     C*
085700990802     C* AZZERO CONTATORE RECORD LETTI
085800990802     C                   MOVEL     '0'           FINTA4            1
085900990802     C*
086000990802     C* POSIZIONAMENTO E PRIMA LETTURA
086100990802     C                   Z-ADD     TASAAS        TA4AAS
086200990802     C                   Z-ADD     TASLNP        TA4LNP
086300990802     C                   Z-ADD     TASNRS        TA4NRS
086400990802     C                   Z-ADD     TASNSP        TA4NSP
086500001019     C     KEYTA4        CHAIN     TITA430C                           99        *FINE-ARCHIV
086600990802     C     *IN99         IFEQ      '1'
086700990802     C                   MOVEL     '1'           FINTA4
086800160412      * recupero riferimento mittente alfanumerico e natura merce
086900061122     c                   else
087000061122     c                   exsr      recta4
087100001019     C                   ENDIF
087200990802     C*
087300990802     C                   ENDSR
087400990802     C*------------------------------------------------------------------------*
087500990802     C*? LETTA4 - LETTURA PROSSIMO RECORD TITA4 - RIFERIMENTI
087600990802     C*------------------------------------------------------------------------*
087700990802     C     LETTA4        BEGSR
087800010124     C*
087900010124     C                   READ      TITA430C                               99
088000990802     C     *IN99         IFEQ      '1'                                          *FINE FILE
088100010124     C     TA4AAS        ORNE      TASAAS
088200010124     C     TA4LNP        ORNE      TASLNP
088300010124     C     TA4NRS        ORNE      TASNRS
088400010124     C     TA4NSP        ORNE      TASNSP
088500990802     C                   MOVEL     '1'           FINTA4
088600001019     C                   ENDIF
088700061122      * se letto
088800061123     c                   if        finta4 = '0'
088900061122     c                   exsr      recta4
089000061122     c                   endif
089100990802     C*
089200990802     C                   ENDSR
089300061122     C*------------------------------------------------------------------------*
089400061122     C*? RECTA4 - RECUPERO RIFERIMENTO MITTENTE ALFANUMERICO / CODICE FISCALE
089500061122     C*------------------------------------------------------------------------*
089600061122     C     RECTA4        BEGSR
089700061122      * riferimento mittente alfanumerico
089800061122     c                   IF        ta4trc = 'A'
089900061122     c                   movel     ta4not        dta4a
090000061122     c                   movel     §ta4aRMA      Tasrma
090100160412     c                   movel     §ta4anas      Tasnas
090200061122     c                   endif
090300061122      * CODICE FISCALE
090400061122     c                   IF        ta4trc = 'Q'
090500061122     c                   movel     ta4not        dta4q
090600061122      * VERIFICO SE FRANCO O ASSEGNATO
090700061122     c                   If        %subst(tastbl:1:1) = 'F'
090800061122     c                   movel     §TA4QCFM      Tascdf
090900061122     c                   endif
091000061122     c                   If        %subst(tastbl:1:1) = 'A'
091100061122     c                   movel     §TA4QCFD      Tascdf
091200061122     c                   endif
091300061122     c                   endif
091400061122
091500061122     c                   endsr
091600010903     C*------------------------------------------------------------------------*
091700010903     C*? SETTAH - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
091800010903     C*------------------------------------------------------------------------*
091900010903     C     SETTAH        BEGSR
092000010903     C*
092100010903     C* AZZERO CONTATORE RECORD LETTI
092200010903     C                   MOVEL     '0'           FINTAH            1
092300010903     C*
092400010903     C* POSIZIONAMENTO E PRIMA LETTURA
092500010903     C                   Z-ADD     TASAAS        TAHAAS
092600010903     C                   Z-ADD     TASLNP        TAHLNP
092700010903     C                   Z-ADD     TASNRS        TAHNRS
092800010903     C                   Z-ADD     TASNSP        TAHNSP
092900010903     C     KEYTAH        CHAIN     TITAH30C                           99        *FINE-ARCHIV
093000010903     C     *IN99         IFEQ      '1'
093100010903     C                   MOVEL     '1'           FINTAH
093200010903     C                   ENDIF
093300010903     C*
093400010903     C                   ENDSR
093500010903     C*------------------------------------------------------------------------*
093600010903     C*? LETTAH - LETTURA PROSSIMO RECORD TITAH - ESTENSIONE DETTAGLI SEGNACOLLI
093700010903     C*------------------------------------------------------------------------*
093800010903     C     LETTAH        BEGSR
093900010903     C*
094000010903     C                   READ      TITAH30C                               99
094100010903     C     *IN99         IFEQ      '1'                                          *FINE FILE
094200010903     C     TAHAAS        ORNE      TASAAS
094300010903     C     TAHLNP        ORNE      TASLNP
094400010903     C     TAHNRS        ORNE      TASNRS
094500010903     C     TAHNSP        ORNE      TASNSP
094600010903     C                   MOVEL     '1'           FINTAH
094700010903     C                   ENDIF
094800010903     C*
094900010903     C                   ENDSR
095000950307     C*------------------------------------------------------------------------*
095100950307     C*? SETCED - POSIZIONAMENTO E PRIMA LETTURA DEL FILE ECCED
095200950307     C*------------------------------------------------------------------------*
095300950307     C     SETCED        BEGSR
095400950307     C*
095500950307     C* AZZERO CONTATORE RECORD LETTI
095600950307     C                   MOVEL     '0'           FINCED            1
095700950307     C*
095800950307     C* POSIZIONAMENTO E PRIMA LETTURA
095900950307     C                   Z-ADD     TASAAS        CEDAAS
096000950307     C                   Z-ADD     TASLNP        CEDLNP
096100950307     C                   Z-ADD     TASNRS        CEDNRS
096200950307     C                   Z-ADD     TASNSP        CEDNSP
096300950307     C                   MOVEL     TASTBL        CEDTBL
096400001019     C     KEYCED        CHAIN     ECCED30C                           99        *FINE-ARCHIV
096500950307     C     *IN99         IFEQ      '1'
096600950307     C                   MOVEL     '1'           FINCED
096700001019     C                   ENDIF
096800950307     C*
096900950307     C                   ENDSR
097000950307     C*------------------------------------------------------------------------*
097100950307     C*? LETCED - LETTURA PROSSIMO RECORD ECCED
097200950307     C*------------------------------------------------------------------------*
097300950307     C     LETCED        BEGSR
097400950307     C*
097500010124     C                   READ      ECCED30C                               99
097600950307     C     *IN99         IFEQ      '1'                                          *FINE FILE
097700010124     C     CEDAAS        ORNE      TASAAS
097800010124     C     CEDLNP        ORNE      TASLNP
097900010124     C     CEDNRS        ORNE      TASNRS
098000010124     C     CEDNSP        ORNE      TASNSP
098100010124     C     CEDTBL        ORNE      TASTBL
098200950307     C                   MOVEL     '1'           FINCED
098300001019     C                   ENDIF
098400950307     C*
098500950307     C                   ENDSR
098600950307     C*------------------------------------------------------------------------*
098700990802     C*? CHKTAS - CONTROLLA VALIDITA' DEL RECORD DI TITAS
098800950307     C*------------------------------------------------------------------------*
098900950309     C     CHKTAS        BEGSR
099000950307     C*
099100950307     C                   MOVEL     'S'           WRECOK            1
099200000329     C*
099300001023     C* CONTROLLA LO STATO DELLA CONSEGNA IN RELAZIONE AL TIPO BOLLA
099400001023     C* ==> SE TRATTASI DI BOLLA DI RECUPERO (F3/A3/AP) STORICIZZO COMUNQUE
099500001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
099600001018     C     TASDCM        IFEQ      *ZEROS                                       *NON CONSEGNATA
099700001023     C     TASTBL        ANDNE     'F3'
099800001023     C     TASTBL        ANDNE     'A3'
099900001023     C     TASTBL        ANDNE     'AP'
100000010222     C     TASCBO        ANDNE     'E '
100100010222     C     TASCBO        ANDNE     'B '
100200001019     C                   EXCEPT    ERRDCM
100300000329     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
100400001018     C                   ENDIF
100500001018     C                   ENDIF
100600950309     C*
100700950309     C* CONTROLLA IL CONTRASSEGNO
100800001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
100900950309     C     TASFCA        IFEQ      'S'                                          --- 1 -->
101000950309     C                   Z-ADD     TASAAS        CSBAAS
101100950309     C                   Z-ADD     TASLNP        CSBLNP
101200950309     C                   Z-ADD     TASNRS        CSBNRS
101300950309     C                   Z-ADD     TASNSP        CSBNSP
101400950309     C     KEYCSB        CHAIN     TNCSB03L                           99
101500950309     C     *IN99         IFEQ      '0'                                          --- 2 -->
101600950309     C     CSBSTA        IFNE      9                                            --- 3 -->
101700950309     C     CSBBNA        IFEQ      *ZEROS                                       *NON ANCORA PAGATO
101800950309     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
101900021021     C***                EXCEPT    NOCCSB                                       *STAMPO RECORD
102000021021     C                   ADD       1             CNTCSB                         *CONTEGGIO RECORD
102100950309     C                   ENDIF
102200950309     C                   ENDIF                                                  <-- 3 ---
102300950309     C                   ENDIF                                                  <-- 2 ---
102400950309     C                   ENDIF                                                  <-- 1 ---
102500001018     C                   ENDIF                                                  <-- 1 ---
102600000327     C*
102700000327     C* CONTROLLA I DANNI
102800001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
102900001018     C     TASFDA        IFEQ      'S'
103000040715      **
103100040715      **  Sostituito con il pgm FIDN12R che restituisce l'informazione
103200040715      **   sui danni fra bolla madre e figlia
103300040715      **
103400040715     C**                 Z-ADD     TASAAS        KCTAAS
103500040715     C**                 Z-ADD     TASLNP        KCTLNP
103600040715     C**                 Z-ADD     TASNRS        KCTNRS
103700040715     C**                 Z-ADD     TASNSP        KCTNSP
103800040715     C**   KEYDCT        CHAIN     FNDCT02L                           99
103900040715      *
104000040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
104100040715      * SIA COME MAMMA CHE COME FIGLIA
104200040715     c                   clear                   fidn12ds
104300040715     c                   eval      i12aas = TASAAS
104400040715     c                   eval      i12lnp = TASLNP
104500040715     c                   eval      i12nrs = TASNRS
104600040715     c                   eval      i12nsp = TASNSP
104700040715     c                   eval      i12fel = 'E'
104800040715     c                   call      'FIDN12R'
104900040715     c                   parm                    fidn12ds
105000040715      **
105100040715      * se c'è almeno una C.A. aperta
105200040715      **
105300040715     c     o12nca        ifgt      0
105400040715     C**   *IN99         IFEQ      *OFF                                         *TROVATO DANNO
105500040715      **
105600000327     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
105700021021     C***                EXCEPT    NOCDCT                                       *STAMPO RECORD
105800021021     C                   ADD       1             CNTDCT                         *CONTEGGIO RECORD
105900001018     C                   ENDIF
106000001018     C                   ENDIF
106100001018     C                   ENDIF
106200000404     C*
106300000404     C* CONTROLLA LE GIACENZE
106400001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
106500001018     C     TASFGC        IFEQ      'S'
106600000404     C                   Z-ADD     TASAAS        KCPAAS
106700000404     C                   Z-ADD     TASLNP        KCPLNP
106800000404     C                   Z-ADD     TASNRS        KCPNRS
106900000404     C                   Z-ADD     TASNSP        KCPNSP
107000000404     C                   Z-ADD     0             KCPFRG                         *PRIMO PRG
107100050301     C     KEYGCP        CHAIN(N)  TIGCP51L                           99
107200001018     C     *IN99         IFEQ      *OFF
107300001018     C     GCPFAS        IFLT      50                                           *FASI APERTE
107400000404     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
107500021021     C***                EXCEPT    NOCGCP                                       *STAMPO RECORD
107600021021     C                   ADD       1             CNTGCP                         *CONTEGGIO RECORD
107700001018     C                   ENDIF
107800001018     C                   ENDIF
107900001018     C                   ENDIF
108000001018     C                   ENDIF
108100011227     C*
108200020220     C* SE BOLLA POSTA CANCELLARE SOLO SE + VECCHIA D 2 ANNI
108300011227     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
108400011227     C     TASTSP        IFEQ      'P'                                          *BOLLA POSTA
108500020220     C     TASDSP        ANDGT     DSDATFP
108600011227     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
108700011227     C                   ENDIF
108800011227     C                   ENDIF
108900001019     C*
109000140109     C* DEVE Controllare: in caso di RESO controlla se la figlia è stata consegnata
109100140109     C*  altrimenti NON deve cancellare la MAMMA.
109200140109     C*
109300131216     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
109400131216     C                   IF        tasCCA = '2' or tasCCA = '6'                 *BOLLA RESO
109500131216     **
109600131223     C* Chiamata metodo GetLastChild
109700131223     c                   exsr      CHKCONS
109800140102     C                   IF         Consegnata_OK <> 'S'
109900140102     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
110000140102     C                   ENDIF
110100131223     **
110200131216     C                   ENDIF
110300131216     C                   ENDIF
110400131216     C*
110500131216     C*
110600131216     C*
110700001019     C* CONTEGGIO BOLLE SCARTATE
110800001019     C     WRECOK        IFEQ      'N'
110900001019     C                   ADD       1             NUMNON
111000001019     C                   ENDIF
111100950307     C*
111200950307     C                   ENDSR
111300131223     C*------------------------------------------------------------------------*
111400131223     C* CHKCONS  - Verifica se bolla effettivamente consegnata
111500131223     C*------------------------------------------------------------------------*
111600131223     C     CHKCONS       BEGSR
111700131223     C*
111800140102     C                   movel     'N'           Consegnata_OK     1
111900131223     C                   movel     *blanks       wEsito1           1
112000131223     C                   movel     *blanks       wEsito2           1
112100131223     C*
112200131223     C                   eval      wAAS = tasAAS
112300131223     C                   eval      wLNP = tasLNP
112400131223     C                   eval      wNRS = tasNRS
112500131223     C                   eval      wNSP = tasNSP
112600131223     C*
112700131223     C* Chiamata metodo GetLblTyp
112800131223     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
112900131223     C                                                wAAS
113000131223     C                                               :wLNP
113100131223     C                                               :wNRS
113200131223     C                                               :wNSP
113300131223     C                                               :pOutLblTyp
113400131223     C                                               :pOutAnnoBO
113500131223     C                                               :pOutLineaParBO
113600131223     C                                               :pOutSerieBO
113700131223     C                                               :pOutNumSpedBO
113800131223     C                                               :pOutDcmBO
113900131223     C                                               :pOutCcaBO
114000131223     C                                               :pOutRblBO))
114100131223     C* Se trattasi d bolla nn originale reperisco la relativa bolla originale
114200131223     C                   if        pOutLblTyp <> 'O'
114300131223     C                   eval      wAAS = pOutAnnoBO
114400131223     C                   eval      wLNP = pOutLineaParBO
114500131223     C                   eval      wNRS = pOutSerieBO
114600131223     C                   eval      wNSP = pOutNumSpedBO
114700131223     C                   endif
114800131223     C*
114900131223     C* Chiamata metodo GetLastChild
115000131223     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
115100131223     C                                                wAAS
115200131223     C                                               :wLNP
115300131223     C                                               :wNRS
115400131223     C                                               :wNSP
115500131223     C                                               :pOutAnnoFI
115600131223     C                                               :pOutLineaParFI
115700131223     C                                               :pOutSerieFI
115800131223     C                                               :pOutNumSpedFI
115900131223     C                                               :pOutDcmFI
116000131223     C                                               :pOutCcaFI))
116100131223     C*
116200140102     C* Considerazioni sullo stato di Consegna della bolla
116300140102     C                   if        (wEsito2 = '9' and pOutDcmBO > *zeros)
116400140102     C                               or
116500131223     C                             (wEsito2 = '0' and pOutDcmFI > *zeros)
116600140102     C                   movel     'S'           Consegnata_OK     1
116700131223     C                   endif
116800131223     C*
116900131223     C                   ENDSR
117000950309     C*------------------------------------------------------------------------*
117100950309     C*?AGGTAB - AGGIORNA LA TABELLA '$3' CON ULTIMA DATA TRASF. TAS00 > TAS10
117200950309     C*------------------------------------------------------------------------*
117300950309     C     AGGTAB        BEGSR
117400950309     C*
117500950309     C                   OPEN      TABEL00F
117600950309     C*
117700950309     C                   Z-ADD     1             KTBUTE
117800950309     C                   MOVEL     '$3'          KTBCOD
117900950309     C                   MOVEL     '1'           KTBKEY
118000950309     C     KEYTAB        CHAIN     TABEL00F                           99
118100950309     C     *IN99         IFEQ      *ON                                          *NON TROVATA
118200950309     C                   ELSE                                                   *TROVATA
118300950309     C                   MOVEL     TBLUNI        DS$3
118400950309     C                   Z-ADD     PARDTF        §$3DP0                         *ULT.DATA TRASF P>SA
118500020220     C                   Z-ADD     PARDFP        §$3DPP                         *ULT.DATA TRASF P>SA
118600950309     C                   MOVEL     DS$3          TBLUNI
118700950309     C                   UPDATE    TABEL
118800950309     C                   ENDIF
118900950309     C*
119000950309     C                   CLOSE     TABEL00F
119100950309     C*
119200950309     C                   ENDSR
119300950717     C*------------------------------------------------------------------------*
119400950717     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
119500950717     C*------------------------------------------------------------------------*
119600950717     C     SUBDAT        BEGSR
119700950717     C                   TIME                    DTTIME
119800950717     C                   Z-ADD     DTANN         DSANN
119900950717     C                   Z-ADD     DTMES         DSMES
120000950717     C                   Z-ADD     DTGIO         DSGIO
120100950717     C                   Z-ADD     DTSEC         DSSEC
120200950717     C                   ENDSR
120300940912     C*------------------------------------------------------------------------*
120400050705     C*? funzione per scivere file x Laguna
120500940912     C*------------------------------------------------------------------------*
120600050705     C     srlaguna      BEGSR
120700050705     c                   clear                   tnsb34ds
120800050929     c* ricavo il progressivo da impostare nella directory ogni 450000 bolle
120900050720     c* cancellate cambio directory
121000120709      * 09/07/2012 con Bocchi verificato che il progressivo non serve più
121100120709     C**!!!              ADD       1             NUMlag                         *REC TRASF TITAS
121200120709     c**!!!numlag        div       450000        sb34prg
121300120709     c**!!!              add       1             sb34prg
121400050705     c                   eval      sb34tla = 'R'
121500050705     c                   eval      sb34aas = tasaas
121600050705     c                   eval      sb34lnp = taslnp
121700050705     c                   eval      sb34nrs = tasnrs
121800050705     c                   eval      sb34nsp = tasnsp
121900150123      *
122000150123      ** andando a pulire di 2 mesi in 2 mesi
122100150123      ** occorre introdurre un nuovo ragionamento sul nome della DIRECTORY di LAGUNA
122200150123      ** determinata nel TNSB34R sulla base della DATA passata in "SB34DTP"
122300050706     c                   eval      sb34dtp = pardtf
122400150123      *
122500150302      *  COME PER LAGUNA anche per archiviare i DOCUMENTI
122600150123      *  Se la spedizione non appartiene all'Anno/Mese della data "FINO AL" (PARDTF)
122700150123      *   del range di pulizia passato (pardti - PARDTF = dal-al), si deve
122800150123      **  sottrarre un mese alla data SB34DTP in modo che LAGUNA imposti il nome
122900150123      **  della Directory sul primo mese dei 2 che si sta pulendo.
123000150123     c                   if        TASanme <> PARDTFanme
123100150123     c                   eval      sb34dtp = pardtf_1m
123200150123     c                   end
123300150123      *
123400050706     c                   eval      sb34cop = parcop
123500050705     c                   call      'TNSB34R'
123600050705     c                   parm                    tnsb34ds
123700050705     c                   if        sb34err <> ' '
123800050705     c                   end
123900050705     C                   ENDSR
124000120709     C*------------------------------------------------------------------------*
124100120709     C*? funzione per scivere file x JDOC
124200120709     C*------------------------------------------------------------------------*
124300120709     C     srJdoc        BEGSR
124400120709     c                   clear                   tnsb34ds
124500120709     c                   eval      sb34tla = 'R'
124600120709     c                   eval      sb34aas = tasaas
124700120709     c                   eval      sb34lnp = taslnp
124800120709     c                   eval      sb34nrs = tasnrs
124900120709     c                   eval      sb34nsp = tasnsp
125000120709     c                   eval      sb34dtp = pardtf
125100150302      *
125200150302      ** Prima andando mese x mese NON esisteva il problema:
125300150302      *  Se la spedizione non appartiene all'Anno/Mese della data "FINO AL" (PARDTF)
125400150302      *   del range di pulizia passato (pardti - PARDTF = dal-al), si deve
125500150302      **  sottrarre un mese alla data SB34DTP come fatto x LAGUNA impostando il nome
125600150302      **  della Directory sul primo mese dei 2 che si sta pulendo.
125700150302     c                   if        TASanme <> PARDTFanme
125800150302     c                   eval      sb34dtp = pardtf_1m
125900150302     c                   end
126000150302      *
126100120709     c                   eval      sb34cop = parcop
126200120709     c                   call      'TNSB34R1'
126300120709     c                   parm                    tnsb34ds
126400120709     c                   if        sb34err <> ' '
126500120709     c                   end
126600120709     C                   ENDSR
126700061122     C*------------------------------------------------------------------------*
126800061122     C*? WRTTASSTO - SCRITTURA FILE STORICO TITASS
126900061122     C*------------------------------------------------------------------------*
127000061122     C     WRTTASSTO     BEGSR
127100080312     c* memorizzo directory laguna per sapere dove è stata archiviata la spe
127200080312     c                   clear                   tasdtfp
127300080312     c                   if        §floiml <> ' '
127400080312     c                   move      tasaas        ladaas
127500080312     c                   move      taslnp        ladlnp
127600080312     c                   move      tasnrs        ladnrs
127700080312     c                   move      tasnsp        ladnsp
127800080312     c     klad          chain     tilad01l
127900080312     c                   If        %found(tilad01l)
128000080312     c                   movel     laddir        tasdtfp
128100080312     c                   end
128200080312     c                   end
128300061122      * Decodifica padroncino
128400061122     c                   If        taspdc <> *zeros
128500061122     c     Keyapd        chain     fiapd01l
128600061122     c                   If        %found(fiapd01l)
128700061122     c                   eval      taspdd = apdrsc
128800061122     c                   endif
128900061122     c                   endif
129000061122
129100061122      * recupero c/assegno
129200061122     c                   if        tasfca = 'S'
129300061122     c                   z-add     csbcas        Tascas
129400061122     c                   movel     csbvca        tasvca
129500061122     c                   endif
129600061122
129700061122     c                   move      tasksc        com04
129800061122      * PARTITA IVA / CODICE FISCALE
129900061122     c                   If        %subst(tastbl:1:1) = 'A'  and
130000061122     c                             tascpd <> *blanks
130100061122     c                   eval      tasiva  = tascpd
130200061122     c                   endif
130300061122      * se partita iva non valorizzata
130400061122     c                   If        tasiva = *blanks  or tascdf = *blanks
130500061122      * codificato franco o assegnato
130600061122     c                   if        com04  <>  9999  and com04  <>  8888
130700061122     c                   exsr      reciva
130800061122     c                   if        o69err <> *on
130900061122     c                   if        tascdf = *blanks
131000061122     c                   eval      tasiva = indiva
131100061122     c                   endif
131200061122      * se devo cercare anche il codife fiscale
131300061122     c                   if        tascdf = *blanks
131400061122     c                   eval      tascdf = indcdf
131500061122     c                   endif
131600061122
131700061122     c                   endif
131800061122     c                   endif
131900061122     c                   endif
132000061124      * valorizzo il flag che mi identifica le spedizioni senza segnacollo
132100091229     c                   If        Tasfns <> 'S' and tasncd = *zeros
132200061124     c                   eval      tassss = 'S'
132300061124     c                   endif
132400061122
132500160927      * verifico se la ds dar5fat è valorizzata
132600160927     c                   If        §ar5pktas > 0
132700160927     c                   eval      taspkf = §ar5pktas
132800160927     c                   endif
132900160927
133000160927     c                   If        §ar5vltas > 0
133100160927     c                   eval      tasvlf = §ar5vltas
133200160927     c                   endif
133300160927
133400061122     c                   write     titassto                             99
133500061122     C   99              EXCEPT    ERRsto
133600061122     C   99              MOVEL     '1'           FINL00                         *FINE PGM
133700061122     C  N99              ADD       1             NUMsto                         *REC TRASF FNEVB
133800061122
133900061122     C                   ENDSR
134000061122     C*
134100050705     C*------------------------------------------------------------------------*
134200050705     C*? RUTINZ - ROUTINE INIZIALE
134300050705     C*------------------------------------------------------------------------*
134400050705     C     RUTINZ        BEGSR
134500931115     C*
134600931115     C* PARAMETRI IN INPUT AL PGM
134700931115     C     *ENTRY        PLIST
134800931115     C                   PARM                    KPJBA
134900931115     C                   MOVEL     KPJBU         PARAM
135000950309     C*
135100150123     C*  Avendo cambiato il modo di pulire da mese x mese in due mesi in due mesi
135200150123     C*   devo preparare una data che verrà utilizzata da Laguna che corrisponde
135300150123     C*   alla data finale del range passato meno 1 mese -->  quindi la data di
135400150123     C*   fine mese del mese precedente a quella del range (PARDTF-1m)
135500150128     c                   move      parDTF        Data08            8 0
135600150128     c                   move      '01'          Data08
135700150128     c                   move      Data08        Data_ISO
135800150128      * prende il fine mese precedente e se lo memorizza
135900150128     c                   subdur    1:*DAYS       Data_ISO
136000150123     c                   move      Data_ISO      parDTF_1m
136100150123     C*
136200150128     C*
136300950309     C* DEFINISCO CAMPI
136400950309     C     *LIKE         DEFINE    TBLKUT        KTBUTE
136500950309     C     *LIKE         DEFINE    TBLCOD        KTBCOD
136600950309     C     *LIKE         DEFINE    TBLKEY        KTBKEY
136700970903     C     *LIKE         DEFINE    CETNBC        MINNBC
136800040715     C***  *LIKE         DEFINE    DCTAAS        KCTAAS                         *FNDCT00F
136900040715     C***  *LIKE         DEFINE    DCTLNP        KCTLNP
137000040715     C***  *LIKE         DEFINE    DCTNRS        KCTNRS
137100040715     C***  *LIKE         DEFINE    DCTNSP        KCTNSP
137200050301     C     *LIKE         DEFINE    GCPAAS        KCPAAS                         *TIGCP00F
137300000404     C     *LIKE         DEFINE    GCPLNP        KCPLNP
137400000404     C     *LIKE         DEFINE    GCPNRS        KCPNRS
137500000404     C     *LIKE         DEFINE    GCPNSP        KCPNSP
137600000404     C     *LIKE         DEFINE    GCPFRG        KCPFRG
137700050301     C     *LIKE         DEFINE    GAPAAS        KAPAAS                         *TIGAP00F
137800000404     C     *LIKE         DEFINE    GAPLNP        KAPLNP
137900000404     C     *LIKE         DEFINE    GAPNRS        KAPNRS
138000000404     C     *LIKE         DEFINE    GAPNSP        KAPNSP
138100050301     C     *LIKE         DEFINE    GNPAGC        KNPAGC                         *TIGNP00F
138200000404     C     *LIKE         DEFINE    GNPFGC        KNPFGC
138300000404     C     *LIKE         DEFINE    GNPNGC        KNPNGC
138400950309     C*
138500950309     C* CHIAVE LETTURA TABEL00F - COMPLETA
138600950309     C     KEYTAB        KLIST
138700950309     C                   KFLD                    KTBUTE
138800950309     C                   KFLD                    KTBCOD
138900950309     C                   KFLD                    KTBKEY
139000080925     C     KEYTAB1       KLIST
139100080925     C                   KFLD                    KTBUTE
139200080925     C                   KFLD                    KTBCOD
139300080925     C                   Z-ADD     1             KTBUTE
139400080925     C                   MOVEL     'TB'          KTBCOD
139500950309     C*
139600950309     C* CHIAVE LETTURA TNCSB01L - COMPLETA
139700950309     C     KEYCSB        KLIST
139800950309     C                   KFLD                    CSBAAS
139900950309     C                   KFLD                    CSBLNP
140000950309     C                   KFLD                    CSBNRS
140100950309     C                   KFLD                    CSBNSP
140200000327     C*
140300000327     C* CHIAVE LETTURA FNDCT02L - COMPLETA
140400040715     C***  KEYDCT        KLIST
140500040715     C***                KFLD                    KCTAAS
140600040715     C***                KFLD                    KCTLNP
140700040715     C***                KFLD                    KCTNRS
140800040715     C***                KFLD                    KCTNSP
140900930208     C*
141000990802     C* CHIAVE LETTURA TITAS38C - PARZIALE
141100930621     C     KEYTAS        KLIST
141200941123     C                   KFLD                    TASAAS
141300941123     C                   KFLD                    TASMGS
141400950307     C*
141500990802     C* CHIAVE LETTURA TITA730C - COMPLETA
141600950307     C     KEYTA7        KLIST
141700950307     C                   KFLD                    TA7AAS
141800950307     C                   KFLD                    TA7LNP
141900950307     C                   KFLD                    TA7NRS
142000950307     C                   KFLD                    TA7NSP
142100950307     C                   KFLD                    TA7TBL
142200990802     C*
142300990802     C* CHIAVE LETTURA TITAA30C - COMPLETA
142400990802     C     KEYTAA        KLIST
142500990802     C                   KFLD                    TAAAAS
142600990802     C                   KFLD                    TAALNP
142700990802     C                   KFLD                    TAANRS
142800990802     C                   KFLD                    TAANSP
142900990802     C*
143000990802     C* CHIAVE LETTURA TITAT30C - COMPLETA
143100990802     C     KEYTAT        KLIST
143200990802     C                   KFLD                    TATAAS
143300990802     C                   KFLD                    TATLNP
143400990802     C                   KFLD                    TATNRS
143500990802     C                   KFLD                    TATNSP
143600990802     C*
143700990802     C* CHIAVE LETTURA TITA430C - COMPLETA
143800990802     C     KEYTA4        KLIST
143900990802     C                   KFLD                    TA4AAS
144000990802     C                   KFLD                    TA4LNP
144100990802     C                   KFLD                    TA4NRS
144200990802     C                   KFLD                    TA4NSP
144300010903     C*
144400010903     C* CHIAVE LETTURA TITAH30C - COMPLETA
144500010903     C     KEYTAH        KLIST
144600010903     C                   KFLD                    TAHAAS
144700010903     C                   KFLD                    TAHLNP
144800010903     C                   KFLD                    TAHNRS
144900010903     C                   KFLD                    TAHNSP
145000950209     C*
145100950307     C* CHIAVE LETTURA ECCET30C - COMPLETA
145200950220     C     KEYCET        KLIST
145300950220     C                   KFLD                    CETAAS                         ANNO SPEDIZIONE
145400950220     C                   KFLD                    CETLNP                         LINEA DI PARTENZA
145500950220     C                   KFLD                    CETNRS                         NUMERO SERIE
145600950220     C                   KFLD                    CETNSP                         NUMERO SPEDIZIONE
145700950220     C                   KFLD                    CETTBL                         TIPO BOLLA
145800950307     C*
145900950307     C* CHIAVE LETTURA ECCED30C - COMPLETA
146000950307     C     KEYCED        KLIST
146100950307     C                   KFLD                    CEDAAS                         ANNO SPEDIZIONE
146200950307     C                   KFLD                    CEDLNP                         LINEA DI PARTENZA
146300950307     C                   KFLD                    CEDNRS                         NUMERO SERIE
146400950307     C                   KFLD                    CEDNSP                         NUMERO SPEDIZIONE
146500950307     C                   KFLD                    CEDTBL                         TIPO BOLLA
146600980325     C*
146700000327     C*
146800000327     C* CHIAVE LETTURA FNEVB01L - COMPLETA
146900000327     C     KEYEVB        KLIST
147000000327     C                   KFLD                    EVBAAS                         ANNO SPEDIZIONE
147100000327     C                   KFLD                    EVBLNP                         LINEA DI PARTENZA
147200000327     C                   KFLD                    EVBNRS                         NUMERO SERIE
147300000327     C                   KFLD                    EVBNSP                         NUMERO SPEDIZIONE
147400000404     C*
147500050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
147600000404     C     KEYGCP        KLIST
147700000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
147800000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
147900000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
148000000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
148100000404     C                   KFLD                    KCPFRG                         PRG RIAPERTURE
148200000404     C*
148300050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
148400000404     C     KE2GCP        KLIST
148500000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
148600000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
148700000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
148800000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
148900000404     C*
149000050301     C* CHIAVE LETTURA TIGAP01L - PARZIALE
149100000404     C     KEYGAP        KLIST
149200000404     C                   KFLD                    KAPAAS                         ANNO SPEDIZIONE
149300000404     C                   KFLD                    KAPLNP                         LINEA DI PARTENZA
149400000404     C                   KFLD                    KAPNRS                         NUMERO SERIE
149500000404     C                   KFLD                    KAPNSP                         NUMERO SPEDIZIONE
149600000404     C*
149700050301     C* CHIAVE LETTURA TIGNP01L - PARZIALE
149800000404     C     KEYGNP        KLIST
149900000404     C                   KFLD                    KNPAGC                         ANNO APERTURA
150000000404     C                   KFLD                    KNPFGC                         FILILAE APERTURA
150100000404     C                   KFLD                    KNPNGC                         NUMERO GIACENZA
150200040303     C*
150300040531     C* CHIAVE LETTURA FIAR531C
150400040303     C     KEYAR5        KLIST
150500040303     C                   KFLD                    AR5AAS
150600040303     C                   KFLD                    AR5LNP
150700040303     C                   KFLD                    AR5NRS
150800040303     C                   KFLD                    AR5NSP
150900120709
151000120709     C     KEYAR51       KLIST
151100120709     C                   KFLD                    AR5AAS
151200120709     C                   KFLD                    AR5LNP
151300120709     C                   KFLD                    AR5NRS
151400120709     C                   KFLD                    AR5NSP
151500120709     C                   KFLD                    AR5TRD
151600060919     C*
151700060919     C* CHIAVE LETTURA TITAI01L
151800060919     C     KEYtai        KLIST
151900060919     C                   KFLD                    taiAAS
152000060919     C                   KFLD                    taiLNP
152100060919     C                   KFLD                    taiNRS
152200060919     C                   KFLD                    taiNSP
152300060919     C                   KFLD                    taiTBL
152400081103
152500081103     C* CHIAVE LETTURA TITAI01L
152600081103     C     KEYrab        KLIST
152700081103     C                   KFLD                    rabAAS
152800081103     C                   KFLD                    rabLNP
152900081103     C                   KFLD                    rabNRS
153000081103     C                   KFLD                    rabNSP
153100061122
153200061122     C* CHIAVE LETTURA Fiapd01l
153300061122     C     KEYapd        KLIST
153400061122     C                   KFLD                    trcapd
153500061122     C                   KFLD                    taspdc
153600040823      *
153700040823     C     kbol          klist
153800040823     C                   kfld                    tasaas
153900040823     C                   kfld                    taslnp
154000040823     C                   kfld                    tasnrs
154100040823     C                   kfld                    tasnsp
154200080312      *
154300080312     C     klad          klist
154400080312     C                   kfld                    LADAAS
154500080312     C                   kfld                    LADLNP
154600080312     C                   kfld                    LADNRS
154700080312     C                   kfld                    LADNSP
154800000327     C*
154900131029     C     Kevb2f        KLIST
155000131029     C                   KFLD                    EVBAAS
155100131029     C                   KFLD                    EVBLNP
155200131029     C                   KFLD                    EVBNRS
155300131029     C                   KFLD                    EVBNSP
155400131029     C                   KFLD                    EVBDTV
155500131029     C                   KFLD                    EVBORV
155600131029     C                   KFLD                    EVBCEV
155700000327     C* CALCOLA LA DATA CORRENTE
155800000327     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
155900000327     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
156000000327     C                   Z-ADD     N8            G08DAT
156100000327     C                   Z-ADD     *ZEROS        G08INV
156200000327     C                   MOVEL     '0'           G08ERR
156300000327     C                   CALL      'XSRDA8'
156400000327     C                   PARM                    WLBDA8
156500000327     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
156600930208     C*
156700930208     C* IMPOSTO A ZERO IL FINE PGM
156800930208     C                   MOVEL     '0'           FINL00            1
156900931105     C                   MOVEL     *BLANKS       RSUT             20
157000040922     C                   Z-ADD     *ZEROS        NUMblp            9 0
157100040922     C                   Z-ADD     *ZEROS        NUMarb            9 0
157200950308     C                   Z-ADD     *ZEROS        NUMTAS            9 0
157300120709     C**!!!              Z-ADD     *ZEROS        NUMlag            9 0
157400950308     C                   Z-ADD     *ZEROS        NUMTA7            9 0
157500990802     C                   Z-ADD     *ZEROS        NUMTAA            9 0
157600990802     C                   Z-ADD     *ZEROS        NUMTAT            9 0
157700990802     C                   Z-ADD     *ZEROS        NUMTA4            9 0
157800010903     C                   Z-ADD     *ZEROS        NUMTAH            9 0
157900950307     C                   Z-ADD     *ZEROS        NUMCET            9 0
158000950308     C                   Z-ADD     *ZEROS        NUMCED            9 0
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
175900970903     O          E            ERRECT         1
176000970903     O                       ECTNBC        Z   +  5
176100970903     O                       ECTCMP        Z   +  1
176200970903     O                       ECTDDC            +  1 '    /  /  '
176300970903     O                                         +  1 'ERR. WRITE ECECT   '
176400970903     O*------------------------------------
176500970903     O          E            ERRECD         1
176600970903     O                       ECDNBC        Z   +  5
176700970903     O                       ECDCMP        Z   +  1
176800970903     O                       ECDFRA            +  1
176900970903     O                       ECDIOE            +  1
177000970903     O                       ECDKCC        Z   +  1
177100970903     O                       ECDKSC        Z   +  1
177200970903     O                       ECDRIC            +  1
177300970903     O                       ECDCCF        Z   +  1
177400970903     O                       ECDDDO            +  1 '    /  /  '
177500970903     O                                         +  1 'ERR. WRITE ECECD   '
177600000404     O*------------------------------------
177700000404     O          E            ERREVB         1
177800000404     O                       EVBAAS            +  5
177900000404     O                       EVBLNP            +  1
178000000404     O                       EVBNRS            +  1
178100000404     O                       EVBNSP            +  1
178200000404     O                                         +  1 'ERR. WRITE FNEVB   '
178300000404     O*------------------------------------
178400000404     O          E            ERRGCP         1
178500000404     O                       GCPAAS            +  5
178600000404     O                       GCPLNP            +  1
178700000404     O                       GCPNRS            +  1
178800000404     O                       GCPNSP            +  1
178900050301     O                                         +  1 'ERR. WRITE TIGCP   '
179000050303     O          E            ERRGCP1        1
179100050303     O                       GCPAAS            +  5
179200050303     O                       GCPLNP            +  1
179300050303     O                       GCPNRS            +  1
179400050303     O                       GCPNSP            +  1
179500050303     O                                         +  1 'GIACENZA ANCORA IN FILIALE'
179600000404     O*------------------------------------
179700000404     O          E            ERRGAP         1
179800000404     O                       GAPAAS            +  5
179900000404     O                       GAPLNP            +  1
180000000404     O                       GAPNRS            +  1
180100000404     O                       GAPNSP            +  1
180200050301     O                                         +  1 'ERR. WRITE TIGAP   '
180300000404     O*------------------------------------
180400000404     O          E            ERRGNP         1
180500000404     O                       GNPAGC            +  5
180600000404     O                       GNPFGC            +  4
180700000404     O                       GNPNGC            +  1
180800050301     O                                         +  1 'ERR. WRITE TIGNP   '
180900040303     O*------------------------------------
181000040303     O          E            ERRAR5         1
181100040303     O                       AR5AAS            +  5
181200040303     O                       AR5LNP            +  1
181300040303     O                       AR5NRS            +  1
181400040303     O                       AR5NSP            +  1
181500040303     O                                         +  1 'ERR. WRITE FIAR5   '
181600060919     O          E            ERRtai         1
181700060919     O                       taiAAS            +  5
181800060919     O                       taiLNP            +  1
181900060919     O                       taiNRS            +  1
182000060919     O                       taiNSP            +  1
182100060919     O                       taitbl            +  1
182200060919     O                                         +  1 'ERR. WRITE TITAI   '
182300081103     O          E            ERRrab         1
182400081103     O                       rabAAS            +  5
182500081103     O                       rabLNP            +  1
182600081103     O                       rabNRS            +  1
182700081103     O                       rabNSP            +  1
182800081103     O                                         +  1 'ERR. WRITE FIRAB   '
182900950309     O*------------------------------------
183000000404     O          E            NOCCSB         1
183100950309     O                       CSBAAS            +  5
183200950309     O                       CSBLNP            +  1
183300950309     O                       CSBNRS            +  1
183400950309     O                       CSBNSP            +  1
183500000404     O                                         +  1 'BOLLA NON CANC:TNCSB'
183600000404     O*------------------------------------
183700040715     O*****     E            NOCDCT         1
183800040715     O*****                  DCTAAS            +  5
183900040715     O*****                  DCTLNP            +  1
184000040715     O*****                  DCTNRS            +  1
184100040715     O*****                  DCTNSP            +  1
184200040715     O*****                                    +  1 'BOLLA NON CANC:FNDCT'
184300040715     O          E            NOCDCT         1
184400040715     O                       i12AAS            +  5
184500040715     O                       i12LNP            +  1
184600040715     O                       i12NRS            +  1
184700040715     O                       i12NSP            +  1
184800040715     O                                         +  1 'BOLLA NON CANC:FNDCT'
184900000404     O*------------------------------------
185000000404     O          E            NOCGCP         1
185100000404     O                       GCPAAS            +  5
185200000404     O                       GCPLNP            +  1
185300000404     O                       GCPNRS            +  1
185400000404     O                       GCPNSP            +  1
185500050301     O                                         +  1 'BOLLA NON CANC:TIGCP'
185600040922     O*------------------------------------
185700040922     O          E            ERRblp         1
185800040922     O                       blpAAS            +  5
185900040922     O                       blpLNP            +  1
186000040922     O                       blpNRS            +  1
186100040922     O                       blpNSP            +  1
186200040922     O                                         +  1 'ERR. Trovato reK su FNBLP'
186300040922     O*------------------------------------
186400040922     O          E            ERRarb         1
186500040922     O                       arbAAS            +  5
186600040922     O                       arbLNP            +  1
186700040922     O                       arbNRS            +  1
186800040922     O                       arbNSP            +  1
186900040922     O                                         +  1 'ERR. Trovato reK su FNARB'
187000061122     O          E            ERRSTO         1
187100061122     O                       TASAAS            +  5
187200061122     O                       TASLNP            +  1
187300061122     O                       TASNRS            +  1
187400061122     O                       TASNSP            +  1
187500061122     O                       TASTBL            +  1
187600061122     O                                         +  1 'ERR. WRITE STORICO '
187700040922     O*------------------------------------
187800950308     O          E            STAFIN         1
187900990802     O                                           30 'N.REC.TRASF. TITAS'
188000950308     O                       NUMTAS        2     44
188100950308     O          E            STAFIN         1
188200990802     O                                           30 'N.REC.TRASF. TITA7'
188300950308     O                       NUMTA7        2     44
188400990802     O          E            STAFIN         1
188500990802     O                                           30 'N.REC.TRASF. TITAA'
188600990802     O                       NUMTAA        2     44
188700990802     O          E            STAFIN         1
188800990802     O                                           30 'N.REC.TRASF. TITAT'
188900990802     O                       NUMTAT        2     44
189000990802     O          E            STAFIN         1
189100990802     O                                           30 'N.REC.TRASF. TITA4'
189200990802     O                       NUMTA4        2     44
189300010903     O          E            STAFIN         1
189400010903     O                                           30 'N.REC.TRASF. TITAH'
189500010903     O                       NUMTAH        2     44
189600950308     O          E            STAFIN         1
189700950308     O                                           30 'N.REC.TRASF. ECCET'
189800950308     O                       NUMCET        2     44
189900950308     O          E            STAFIN         1
190000950308     O                                           30 'N.REC.TRASF. ECCED'
190100950308     O                       NUMCED        2     44
190200970903     O          E            STAFIN         1
190300970903     O                                           30 'N.REC.TRASF. ECECT'
190400970903     O                       NUMECT        2     44
190500970903     O          E            STAFIN         1
190600970903     O                                           30 'N.REC.TRASF. ECECD'
190700970903     O                       NUMECD        2     44
190800000327     O          E            STAFIN         1
190900000327     O                                           30 'N.REC.TRASF. FNEVB'
191000000327     O                       NUMEVB        2     44
191100000404     O          E            STAFIN         1
191200050301     O                                           30 'N.REC.TRASF. TIGCP'
191300000404     O                       NUMGCP        2     44
191400000404     O          E            STAFIN         1
191500050301     O                                           30 'N.REC.TRASF. TIGAP'
191600000404     O                       NUMGAP        2     44
191700000404     O          E            STAFIN         1
191800050301     O                                           30 'N.REC.TRASF. TIGNP'
191900000404     O                       NUMGNP        2     44
192000040310     O          E            STAFIN         1
192100040310     O                                           30 'N.REC. BOLLE FIAR5'
192200040310     O                       NUMAR5        2     44
192300060919     O          E            STAFIN         1
192400060919     O                                           30 'N.REC. BOLLE TITAI'
192500060919     O                       NUMTAI        2     44
192600081103     O          E            STAFIN         1
192700081103     O                                           30 'N.REC. BOLLE FIRAB'
192800081103     O                       NUMRAB        2     44
192900061122     O          E            STAFIN         1
193000061122     O                                           30 'N.BOLLE IN STORICO'
193100061122     O                       NUMSTO        2     44
193200001019     O          E            STAFIN         1
193300001020     O                                           30 'N.REC. BOLLE SCART'
193400001019     O                       NUMNON        2     44
193500021021     O          E            STAFIN         1
193600021021     O                                           30 'DI CUI:'
193700021021     O          E            STAFIN         1
193800021021     O                                           30 'X GIACENZA APERTA '
193900021021     O                       CNTGCP        2     44
194000021021     O          E            STAFIN         1
194100021021     O                                           30 'X DANNO PENDENTE  '
194200021021     O                       CNTDCT        2     44
194300021021     O          E            STAFIN         1
194400021021     O                                           30 'X C/A ATTIVO      '
194500021021     O                       CNTCSB        2     44
194600040922     O          E            STAFIN         1
194700040922     O                                           30 'N.REK.Trovati FNBLP'
194800040922     O                       NUMblp        2     44
194900040922     O          E            STAFIN         1
195000040922     O                                           30 'N.REK.Trovati FNARB'
195100040922     O                       NUMarb        2     44
195200060626     O          E            STAFIN         1
195300060626     O                                           +0 'Chiusuara pgm: FINTAS'
195400060626     O                       fintas              +1
195500060626     O                       tasdsp              +1
195600060626     O                       dsdatf              +1
195700060626     O               98                          +1 'SHTDN'
195800060626     O                                           +1 'FINL00'
195900060626     O                       finl00              +1
196000950308     O          E            STAFIN           60
196100931115     O                                           76 '*** FINE STAMPA ***'
196200931115     O                                          150 'Data:'
196300950717     O                       DSDAT6             159 '  /  /  '
196400990802     O                                          167 'Ora:'
196500950717     O                       DTORA6             178 '  :  :  '
196600940912     O*------------------------------------------------------------------------*
196700970903     O* AGGIORNAMENTO FILES
196800970903     O*------------------------------------------------------------------------*
196900970903     OECECT     E            AGGECT
197000970903     O                       ECTATB
197100970903     O                       ECTFTR
197200970903     OECECD     E            AGGECD
197300970903     O                       ECDATB
197400970903     O                       ECDFTR
197500970903     O*------------------------------------------------------------------------*
197600040823** cmd
197700040823OVRDBF FILE(        ) TOFILE(
197800931105**  TES - RIGHE IN STAMPA
197900931105------------------------------------------------------------------  1
198000931105 DATA    ORA       AA LNP SE N.SPED. TB     ERR  DESCRIZIONE ERROR  2
198100950308E                                                                   3
198200931105                                                                    4
198300931105                                                                    5
198400931105                                                                    6
198500931105                                                                    7
198600931105                                                                    8
198700931105                                                                    9
198800931105                                                                   10
198900931105                                                                   11
199000931105                                                                   12
199100931105                                                                   13
199200931105                                                                   14
199300931105                                                                   15
