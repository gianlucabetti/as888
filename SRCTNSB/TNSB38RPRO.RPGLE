000100940912     H*------------------------------------------------------------------------*
000200971105     H* TRASFERIMENTO BOLLE NELLA LIBRERIA DI SALVATAGGIO
000300940912     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500990802     F*---
000600990802     F* BOLLE
000700990802     F*---
000800110224     FTITAS38C  iF   E           K DISK
000900990802     F                                     IGNORE(TITAS000)
001000990802     F                                     IGNORE(TITAS010)
001100990802     FTITASP0F  O  A E           K DISK
001200990802     F                                     RENAME(TITASP00:TITASS00)
001300061121     FTITASS0F  O  A E           K DISK
001400061121     F                                     RENAME(TITASS00:TITASSTO)
001500000404     F*---
001600000404     F* GIACENZE
001700000404     F*---
001800110224     Ftigcp51l  iF   E           K DISK
001900050301     F                                     RENAME(tigcp000:tigcpS)
002000000404     F*---
002100000404     F* GIACENZE -ANAGRAFICHE MITTENTE-
002200000404     F*---
002300110224     Ftigap01l  iF   E           K DISK
002400050301     F                                     RENAME(TIGAP000:TIGAPS)
002500000404     F*---
002600000404     F* GIACENZE -NOTE-
002700000404     F*---
002800110224     FTIGNP01L  iF   E           K DISK
002900050301     F                                     RENAME(TIGNP000:TIGNPS)
003000040303     F*---
003100040303     F* ESTENSIONE ALLA TESTATA BOLLE
003200000404     F*---
003300000327     F* TABELLE
003400000327     F*---
003500110224     FTABEL00F  iF   E           K DISK    USROPN
003600000327     F*---
003700000327     F* CONTRASSEGNI
003800000327     F*---
003900950309     FTNCSB03L  IF   E           K DISK
004000000327     F*---
004100000327     F* DANNI
004200000327     F*---
004300040715     F***FNDCT02L  IF   E           K DISK
004400040823     F*---
004500040823     Ffnblp01L  IF   E           K DISK    usropn
004600040823     Ffnarb01L  IF   E           K DISK    usropn
004700110302     F*---
004800110302     Ffnlbl02l  IF   E           K DISK
004900000327     F*---
005000000327     F* PRINTER FILE
005100000327     F*---
005200931105     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
005300931105     F                                     FORMLEN(66)
005400931105     F                                     FORMOFL(60)
005500940912     L*------------------------------------------------------------------------*
005600040823     d wlib            s             10    inz('FILTRA201 ')
005700040910     d wlibp           s             10    inz('FILTRAPRD ')
005800040823     d comman          s             80
005900040823     d lenght          s             15  5 inz(80)
006000061122     d com04           s              4  0
006100940912     D*------------------------------------------------------------------------*
006200940912     D* SCHIERE
006300940912     D*------------------------------------------------------------------------*
006400040823     d cmd             s             60    dim(01) ctdata perrcd(1)
006500931105     D TES             S             66    DIM(15) CTDATA PERRCD(1)
006600080925     d tbl             s              2    dim(50) inz
006700950309     D* DS TABELLA $3 (DATE AVANZAMENTO TRASFERIMENTO BOLLE)
006800950309     D DS$3          E DS
006900080925     D DStb          E DS
007000950717     D* DS PER SCOMPORRE DATA IN ANNO/MESE/GIONRO
007100950717     D DS§DAT          DS
007200950717     D  DSSEC                  1      2  0
007300950717     D  DSANN                  3      4  0
007400950717     D  DSMES                  5      6  0
007500950717     D  DSGIO                  7      8  0
007600950717     D  DSAAS                  1      4  0
007700950717     D  DSMGS                  5      8  0
007800950717     D  DSDAT6                 3      8  0
007900950717     D  DSDAT8                 1      8  0
008000950717     D DS§TIM          DS
008100950717     D  DTORA                  1      2  0
008200950717     D  DTMIN                  3      4  0
008300950717     D  DTSCN                  5      6  0
008400950717     D  DTGIO                  7      8  0
008500950717     D  DTMES                  9     10  0
008600950717     D  DTSEC                 11     12  0
008700950717     D  DTANN                 13     14  0
008800950717     D  DTORA6                 1      6  0
008900950717     D  DTTIME                 1     14  0
009000940912     D*------------------------------------------------------------------------*
009100940912     D* INPUT
009200940912     D*------------------------------------------------------------------------*
009300941123     D                 DS
009400941123     D  DSAASI                 1      4  0
009500941123     D  DSMGSI                 5      8  0
009600950309     D  DSDATI                 1      8  0
009700941123     D                 DS
009800941123     D  DSAASF                 1      4  0
009900941123     D  DSMGSF                 5      8  0
010000950309     D  DSDATF                 1      8  0
010100020220     D                 DS
010200020220     D  DSAASFP                1      4  0
010300020220     D  DSMGSFP                5      8  0
010400020220     D  DSDATFP                1      8  0
010500960413     D*-------------------
010600960413     D* DATA SPEDIZIONE
010700960413     D*-------------------
010800960413     D                 DS
010900960413     D  TASAAS                 1      4  0
011000960413     D  TASMGS                 5      8  0
011100960413     D  TASDSP                 1      8  0
011200000327     D*------------------
011300000327     D* DS "XSRDA8" - CONTROLLA DATA (8)
011400000327     D*------------------
011500000327     D WLBDA8          DS                  INZ
011600000327     D  G08DAT                 1      8  0
011700000327     D  G08INV                 9     16  0
011800000327     D  G08ERR                17     17
011900000327     D  G08TGI                18     22  0
012000040715
012100040715     d Fidn12ds      e ds
012200040715     d  skKey                 26    305    dim(20)
012300040715     d  skAnn                306    325    dim(20)
012400040715     d  skDca                326    485    dim(20)
012500040715     d  skFca                486    545  0 dim(20)
012600040715     d  skDch                546    705  0 dim(20)
012700040715     d  skCch                706    745    dim(20)
012800040715
012900940912     D*-------------------
013000931115     D* PARAMETRI LANCIO PGM
013100940912     D*-------------------
013200000327     D PARAM           DS
013300950309     D  PARDTI                 1      8  0                                      DT.INI
013400950309     D  PARDTF                 9     16  0                                      DT.FIN
013500020220     D  PARDFP                17     24  0                                      DT.FIN PT
013600050706     D  PARcop                25     27                                         DT.FIN PT
013700940912     D*-------------------
013800940912     D* ARCHITETTURA
013900940912     D*-------------------
014000931115     D KPJBA         E DS
014100050705     D tnsb34ds      E DS
014200050705     D dtasflo       E DS
014300061121      *
014400061121     D*-------------------
014500061121     D* ANAGRAFICA
014600061121     D*-------------------
014700061121     d TIBS69ds      e ds                  inz
014800061121     d DS_cnaco      e ds                  inz  extname(CNACO00F)
014900061121     d DS_cnind      e ds                  inz  extname(CNIND00F)
015000061121     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
015100061121     d DS_fncls      e ds                  inz  extname(FNCLS00F)
015200061122     D*-------------------
015300061122     D* RIFERIMENTI ALFANUMERICI
015400061122     D*-------------------
015500061122     d DTA4A         e ds
015600061122     D*-------------------
015700061122     D* CODICE FISCALE
015800061122     D*-------------------
015900061122     d DTA4Q         e ds
016000940912     C*------------------------------------------------------------------------*
016100940912     C*?MAIN LINE
016200940912     C*------------------------------------------------------------------------*
016300000000     C*
016400930129     C* ROUTINE INIZIALE
016500930129     C                   EXSR      RUTINZ
016600931105     C*
016700931105     C* STAMPA RIGA DI TESTATA
016800931105     C                   EXCEPT    TESTA
016900930601     C*
017000930601     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
017100930601     C     FINL00        IFEQ      '0'                                          --- 1 -->
017200000000     C*
017300990802     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA SU TITAS
017400950307     C                   EXSR      SETTAS
017500930128     C*
017600990802     C* CICLO FINO A FINE FILE TITAS OPPURE
017700950308     C* RICHIESTA CHIUSURA PGM OPPURE
017800950308     C* FINE PGM PER ERRORE
017900950308     C                   SETOFF                                       98
018000950307     C     FINTAS        DOWEQ     '0'                                          --- 2 -->
018100950308     C     *IN98         ANDEQ     '0'
018200950308     C     FINL00        ANDEQ     '0'
018300080214      * verifico   che non esistano ARB e BLP altrimenti non cancello
018400040823      * e leggo un'altro rekord
018500040823     C     Kbol          Chain     fnblp01l                           33
018600040922     C  N33              ADD       1             NUMblp                         *REC TRASF TITA7
018700040922     C  n33              EXCEPT    ERRblp
018800080214     C     kbol          Chain     fnarb01l                           34
018900080214     C  N34              ADD       1             NUMarb                         *REC TRASF TITA7
019000080214     C  n34              EXCEPT    ERRarb
019100080214     c                   if        not *in33 or not *in34
019200080214     c                   goto      altrorek
019300080214     c                   end
019400000000     C*
019500980325     C* LETTURA SUCCESSIVA BOLLE
019600040823     c     altrorek      tag
019700040823
019800060707     c                   if        finl00 = '0'
019900990802     C                   EXSR      LETTAS                                       *SUCCESSIVO TITAS
020000060707     c                   end
020100950308     C                   SHTDN                                        98        *CONTR.CHIUSURA PGM
020200930128     C*
020300950307     C                   ENDDO                                                  <-- 2 ---
020400950307     C                   ENDIF                                                  <-- 1 ---
020500000000     C*
020600000000     C                   SETON                                        LR
020700940912     C*------------------------------------------------------------------------*
020800990802     C*? SETTAS - POSIZIONAMENTO E PRIMA LETTURA DEL FILE TITAS
020900940912     C*------------------------------------------------------------------------*
021000950307     C     SETTAS        BEGSR
021100931115     C*
021200931115     C* AZZERO CONTATORE RECORD LETTI
021300950307     C                   MOVEL     '0'           FINTAS            1
021400930208     C*
021500930316     C* POSIZIONAMENTO E PRIMA LETTURA
021600941123     C                   Z-ADD     PARDTI        DSDATI
021700941123     C                   Z-ADD     PARDTF        DSDATF
021800020220     C                   Z-ADD     PARDFP        DSDATFP
021900941123     C                   Z-ADD     DSAASI        TASAAS
022000941123     C                   Z-ADD     DSMGSI        TASMGS
022100990802     C     KEYTAS        SETLL     TITAS38C                           99        *FINE-ARCHIV
022200930625     C*
022300930625     C     *IN99         IFEQ      '1'
022400950307     C                   MOVEL     '1'           FINTAS
022500930625     C                   ELSE
022600950307     C                   EXSR      LETTAS
022700001020     C                   ENDIF
022800930208     C*
022900930208     C                   ENDSR
023000940912     C*------------------------------------------------------------------------*
023100990802     C*? LETTAS - LETTURA PROSSIMO RECORD TITAS
023200940912     C*------------------------------------------------------------------------*
023300950307     C     LETTAS        BEGSR
023400930625     C*
023500930625     C* LEGGO FINO A:
023600930625     C*     - FINE FILE O SUPERATO LIMITE FINALE
023700930625     C*     - TROVATO RECORD VALIDO
023800930625     C                   MOVEL     'N'           WRECOK
023900950307     C     FINTAS        DOWEQ     '0'                                          --- 1 -->
024000930625     C     WRECOK        ANDEQ     'N'
024100061121      * pulisco lo storico
024200061121     c                   clear                   titassto
024300930208     C*
024400930316     C* LETTURE SUCCESSIVE ALLA PRIMA
024500990802     C                   READ      TITAS38C                               99
024600960413     C*
024700960413     C     *IN99         IFEQ      '1'                                          *FINE FILE
024800020220     C     TASDSP        ORGT      DSDATF                                       *SUPERATA DATA FINE
024900950307     C                   MOVEL     '1'           FINTAS
025000930625     C                   ELSE
025100950309     C                   EXSR      CHKTAS                                       *CONTR.VALIDITA REC.
025200001019     C                   ENDIF
025300930625     C*
025400001019     C                   ENDDO                                                  <-- 1 ---
025500930208     C*
025600930208     C                   ENDSR
025700950307     C*------------------------------------------------------------------------*
025800990802     C*? CHKTAS - CONTROLLA VALIDITA' DEL RECORD DI TITAS
025900950307     C*------------------------------------------------------------------------*
026000950309     C     CHKTAS        BEGSR
026100950307     C*
026200110302     c                   setoff                                       202122
026300110302     c                   setoff                                       23
026400950307     C                   MOVEL     'S'           WRECOK            1
026500110224     c                   clear                   wCA               1
026600110224     C* CONTROLLA I DANNI
026700110224    1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
026800110224    2C     TASFDA        IFEQ      'S'
026900110224      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
027000110224      * SIA COME MAMMA CHE COME FIGLIA
027100110224     c                   clear                   fidn12ds
027200110224     c                   eval      i12aas = TASAAS
027300110224     c                   eval      i12lnp = TASLNP
027400110224     c                   eval      i12nrs = TASNRS
027500110224     c                   eval      i12nsp = TASNSP
027600110224     c                   eval      i12fel = 'E'
027700110224     c                   call      'FIDN12R'
027800110224     c                   parm                    fidn12ds
027900110224      **
028000110224      * se c'è almeno una C.A. aperta
028100110224      **
028200110224    3c     o12nca        ifgt      0
028300110224      **
028400110224     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
028500110224     c* non pulisco, non stampo nulla perchè la bolla DEVE rimanere in TITAS
028600110224     c                   eval      wCA='S'
028700110224     C                   ADD       1             CNTDCT                         *CONTEGGIO RECORD
028800110224    3C                   ENDIF
028900110224    2C                   ENDIF
029000110224    1C                   ENDIF
029100000329     C*
029200001023     C* CONTROLLA LO STATO DELLA CONSEGNA IN RELAZIONE AL TIPO BOLLA
029300001023     C* ==> SE TRATTASI DI BOLLA DI RECUPERO (F3/A3/AP) STORICIZZO COMUNQUE
029400110302    1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
029500110302    2C     TASDCM        IFEQ      *ZEROS                                       *NON CONSEGNATA
029600001023     C     TASTBL        ANDNE     'F3'
029700001023     C     TASTBL        ANDNE     'A3'
029800001023     C     TASTBL        ANDNE     'AP'
029900010222     C     TASCBO        ANDNE     'E '
030000010222     C     TASCBO        ANDNE     'B '
030100110302     c* Verifico se si tratta di un rimpallo --> lo scrivo subito
030200110302     C     kbol          Chain     fnarb01l
030300110302    3c                   if        %found(fnarb01l) and arbfbc='B'
030400110302     c                   seton                                        20
030500110302   x3c                   else
030600110302     c* Verifico se legata a bolla con linea di arrivo 101 - RESI SEDE
030700110302    4c                   if        taslna=101
030800110302     c                   seton                                        21
030900110302   x4c                   else
031000110302     c                   clear                   lblaan
031100110302     c                   clear                   lbllpn
031200110302     c                   clear                   lblnrn
031300110302     c                   clear                   lblnsn
031400110302     c     kbol          chain     fnlbl02l
031500110302    5c                   dow       %found(fnlbl02l) and  lbllan<>101
031600110302     c     klbl          chain     fnlbl02l
031700110302    5c                   enddo
031800110302     c
031900110302    5c                   if        %found(fnlbl02l) and lbllan=101
032000110302     c                   seton                                        22
032100110302   x5c                   else
032200110302     c* Verifico se la figlia è un rimpallo
032300110302     C     klbl          Chain     fnarb01l
032400110302    6c                   if        %found(fnarb01l) and arbfbc='B'
032500110302     c                   seton                                        23
032600110302    6c                   endif
032700110302    5c                   endif
032800110302    4c                   endif
032900110302
033000110302    3c                   endif
033100110302
033200001019     C                   EXCEPT    ERRDCM
033300000329     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
033400110302    2C                   ENDIF
033500110302    1C                   ENDIF
033600950309     C*
033700950309     C* CONTROLLA IL CONTRASSEGNO
033800001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
033900950309     C     TASFCA        IFEQ      'S'                                          --- 1 -->
034000950309     C                   Z-ADD     TASAAS        CSBAAS
034100950309     C                   Z-ADD     TASLNP        CSBLNP
034200950309     C                   Z-ADD     TASNRS        CSBNRS
034300950309     C                   Z-ADD     TASNSP        CSBNSP
034400950309     C     KEYCSB        CHAIN     TNCSB03L                           99
034500950309     C     *IN99         IFEQ      '0'                                          --- 2 -->
034600950309     C     CSBSTA        IFNE      9                                            --- 3 -->
034700950309     C     CSBBNA        IFEQ      *ZEROS                                       *NON ANCORA PAGATO
034800950309     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
034900021021     C***                EXCEPT    NOCCSB                                       *STAMPO RECORD
035000021021     C                   ADD       1             CNTCSB                         *CONTEGGIO RECORD
035100950309     C                   ENDIF
035200950309     C                   ENDIF                                                  <-- 3 ---
035300950309     C                   ENDIF                                                  <-- 2 ---
035400950309     C                   ENDIF                                                  <-- 1 ---
035500001018     C                   ENDIF                                                  <-- 1 ---
035600000404     C*
035700000404     C* CONTROLLA LE GIACENZE
035800001018     C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
035900001018     C     TASFGC        IFEQ      'S'
036000000404     C                   Z-ADD     TASAAS        KCPAAS
036100000404     C                   Z-ADD     TASLNP        KCPLNP
036200000404     C                   Z-ADD     TASNRS        KCPNRS
036300000404     C                   Z-ADD     TASNSP        KCPNSP
036400000404     C                   Z-ADD     0             KCPFRG                         *PRIMO PRG
036500050301     C     KEYGCP        CHAIN(N)  TIGCP51L                           99
036600001018     C     *IN99         IFEQ      *OFF
036700001018     C     GCPFAS        IFLT      50                                           *FASI APERTE
036800000404     C                   MOVEL     'N'           WRECOK                         *NON CANCELLO
036900021021     C***                EXCEPT    NOCGCP                                       *STAMPO RECORD
037000021021     C                   ADD       1             CNTGCP                         *CONTEGGIO RECORD
037100001018     C                   ENDIF
037200001018     C                   ENDIF
037300001018     C                   ENDIF
037400001018     C                   ENDIF
037500001019     C*
037600001019     C* CONTEGGIO BOLLE SCARTATE
037700001019     C     WRECOK        IFEQ      'N'
037800001019     C                   ADD       1             NUMNON
037900001019     C                   ENDIF
038000950307     C*
038100950307     C                   ENDSR
038200950717     C*------------------------------------------------------------------------*
038300950717     C*?SUBDAT - SUBROUTINE PER RECUPERARE DATA E ORA ATTUALI
038400950717     C*------------------------------------------------------------------------*
038500950717     C     SUBDAT        BEGSR
038600950717     C                   TIME                    DTTIME
038700950717     C                   Z-ADD     DTANN         DSANN
038800950717     C                   Z-ADD     DTMES         DSMES
038900950717     C                   Z-ADD     DTGIO         DSGIO
039000950717     C                   Z-ADD     DTSEC         DSSEC
039100950717     C                   ENDSR
039200050705     C*------------------------------------------------------------------------*
039300050705     C*? RUTINZ - ROUTINE INIZIALE
039400050705     C*------------------------------------------------------------------------*
039500050705     C     RUTINZ        BEGSR
039600931115     C*
039700931115     C* PARAMETRI IN INPUT AL PGM
039800931115     C     *ENTRY        PLIST
039900931115     C                   PARM                    KPJBA
040000931115     C                   MOVEL     KPJBU         PARAM
040100950309     C*
040200950309     C* DEFINISCO CAMPI
040300950309     C     *LIKE         DEFINE    TBLKUT        KTBUTE
040400950309     C     *LIKE         DEFINE    TBLCOD        KTBCOD
040500950309     C     *LIKE         DEFINE    TBLKEY        KTBKEY
040600040715     C***  *LIKE         DEFINE    DCTAAS        KCTAAS                         *FNDCT00F
040700040715     C***  *LIKE         DEFINE    DCTLNP        KCTLNP
040800040715     C***  *LIKE         DEFINE    DCTNRS        KCTNRS
040900040715     C***  *LIKE         DEFINE    DCTNSP        KCTNSP
041000050301     C     *LIKE         DEFINE    GCPAAS        KCPAAS                         *TIGCP00F
041100000404     C     *LIKE         DEFINE    GCPLNP        KCPLNP
041200000404     C     *LIKE         DEFINE    GCPNRS        KCPNRS
041300000404     C     *LIKE         DEFINE    GCPNSP        KCPNSP
041400000404     C     *LIKE         DEFINE    GCPFRG        KCPFRG
041500050301     C     *LIKE         DEFINE    GAPAAS        KAPAAS                         *TIGAP00F
041600000404     C     *LIKE         DEFINE    GAPLNP        KAPLNP
041700000404     C     *LIKE         DEFINE    GAPNRS        KAPNRS
041800000404     C     *LIKE         DEFINE    GAPNSP        KAPNSP
041900050301     C     *LIKE         DEFINE    GNPAGC        KNPAGC                         *TIGNP00F
042000000404     C     *LIKE         DEFINE    GNPFGC        KNPFGC
042100000404     C     *LIKE         DEFINE    GNPNGC        KNPNGC
042200950309     C*
042300950309     C* CHIAVE LETTURA TABEL00F - COMPLETA
042400950309     C     KEYTAB        KLIST
042500950309     C                   KFLD                    KTBUTE
042600950309     C                   KFLD                    KTBCOD
042700950309     C                   KFLD                    KTBKEY
042800080925     C     KEYTAB1       KLIST
042900080925     C                   KFLD                    KTBUTE
043000080925     C                   KFLD                    KTBCOD
043100080925     C                   Z-ADD     1             KTBUTE
043200080925     C                   MOVEL     'TB'          KTBCOD
043300950309     C*
043400950309     C* CHIAVE LETTURA TNCSB01L - COMPLETA
043500950309     C     KEYCSB        KLIST
043600950309     C                   KFLD                    CSBAAS
043700950309     C                   KFLD                    CSBLNP
043800950309     C                   KFLD                    CSBNRS
043900950309     C                   KFLD                    CSBNSP
044000000327     C*
044100930208     C*
044200990802     C* CHIAVE LETTURA TITAS38C - PARZIALE
044300930621     C     KEYTAS        KLIST
044400941123     C                   KFLD                    TASAAS
044500941123     C                   KFLD                    TASMGS
044600950307     C*
044700990802     C* CHIAVE LETTURA TITA730C - COMPLETA
044800000404     C*
044900050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
045000000404     C     KEYGCP        KLIST
045100000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
045200000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
045300000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
045400000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
045500000404     C                   KFLD                    KCPFRG                         PRG RIAPERTURE
045600000404     C*
045700050301     C* CHIAVE LETTURA TIGCP51L - PARZIALE
045800000404     C     KE2GCP        KLIST
045900000404     C                   KFLD                    KCPAAS                         ANNO SPEDIZIONE
046000000404     C                   KFLD                    KCPLNP                         LINEA DI PARTENZA
046100000404     C                   KFLD                    KCPNRS                         NUMERO SERIE
046200000404     C                   KFLD                    KCPNSP                         NUMERO SPEDIZIONE
046300000404     C*
046400061122
046500110302     C     klbl          klist
046600110302     C                   kfld                    lblaan
046700110302     C                   kfld                    lbllpn
046800110302     C                   kfld                    lblnrn
046900110302     C                   kfld                    lblnsn
047000040823      *
047100040823     C     kbol          klist
047200040823     C                   kfld                    tasaas
047300040823     C                   kfld                    taslnp
047400040823     C                   kfld                    tasnrs
047500040823     C                   kfld                    tasnsp
047600000327     C*
047700000327     C* CALCOLA LA DATA CORRENTE
047800000327     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
047900000327     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
048000000327     C                   Z-ADD     N8            G08DAT
048100000327     C                   Z-ADD     *ZEROS        G08INV
048200000327     C                   MOVEL     '0'           G08ERR
048300000327     C                   CALL      'XSRDA8'
048400000327     C                   PARM                    WLBDA8
048500000327     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
048600930208     C*
048700930208     C* IMPOSTO A ZERO IL FINE PGM
048800930208     C                   MOVEL     '0'           FINL00            1
048900931105     C                   MOVEL     *BLANKS       RSUT             20
049000040922     C                   Z-ADD     *ZEROS        NUMblp            9 0
049100040922     C                   Z-ADD     *ZEROS        NUMarb            9 0
049200950308     C                   Z-ADD     *ZEROS        NUMTAS            9 0
049300050720     C                   Z-ADD     *ZEROS        NUMlag            9 0
049400950308     C                   Z-ADD     *ZEROS        NUMTA7            9 0
049500990802     C                   Z-ADD     *ZEROS        NUMTAA            9 0
049600990802     C                   Z-ADD     *ZEROS        NUMTAT            9 0
049700990802     C                   Z-ADD     *ZEROS        NUMTA4            9 0
049800010903     C                   Z-ADD     *ZEROS        NUMTAH            9 0
049900950307     C                   Z-ADD     *ZEROS        NUMCET            9 0
050000950308     C                   Z-ADD     *ZEROS        NUMCED            9 0
050100980325     C                   Z-ADD     *ZEROS        NUMFIM            9 0
050200970904     C                   Z-ADD     *ZEROS        NUMECT            9 0
050300970904     C                   Z-ADD     *ZEROS        NUMECD            9 0
050400000327     C                   Z-ADD     *ZEROS        NUMEVB            9 0
050500000404     C                   Z-ADD     *ZEROS        NUMGCP            9 0
050600000404     C                   Z-ADD     *ZEROS        NUMGAP            9 0
050700000404     C                   Z-ADD     *ZEROS        NUMGNP            9 0
050800001019     C                   Z-ADD     *ZEROS        NUMNON            9 0
050900040303     C                   Z-ADD     *ZEROS        NUMAR5            9 0
051000060919     C                   Z-ADD     *ZEROS        NUMtai            9 0
051100081103     C                   Z-ADD     *ZEROS        NUMrab            9 0
051200061122     C                   Z-ADD     *ZEROS        NUMsto            9 0
051300021021     C                   Z-ADD     *ZEROS        CNTGCP            9 0
051400021021     C                   Z-ADD     *ZEROS        CNTDCT            9 0
051500021021     C                   Z-ADD     *ZEROS        CNTCSB            9 0
051600040823      * Apro il Fnblp di filiale
051700040823     c                   Clear                   comman
051800040823     c                   Movel(p)  cmd(1)        comman
051900040823     c                   Eval      %Subst(comman:13:8) = 'FNBLP01L'
052000040910      *test sistema informativo di prova
052100040910     c                   if        %subst(knsif:7:1) = 'P'
052200040910     c                   Eval      %Subst(comman:30:10) = wlibp
052300040910     c                   else
052400040910     c                   Eval      %Subst(comman:30:10) = wlib
052500040910     c                   endif
052600040910
052700040823     c                   Eval      comman =
052800040823     c                             %trim(comman) + '/FNBLP01L)'
052900040823     c                   Call      'QCMDEXC'                            99
053000040823     c                   Parm                    comman
053100040823     c                   Parm                    lenght
053200040823     c                   If        *In99
053300040823     c                   seton                                        lr
053400040823     c                   Else
053500040823     c                   Open      Fnblp01l
053600040823     c                   EndIf
053700040823      *
053800040823      * Apro  Fnarb di filiale
053900040823     c                   Clear                   comman
054000040823     c                   Movel(p)  cmd(1)        comman
054100040823     c                   Eval      %Subst(comman:13:8) = 'FNARB01L'
054200040910      *test sistema informativo di prova
054300040910     c                   if        %subst(knsif:7:1) = 'P'
054400040910     c                   Eval      %Subst(comman:30:10) = wlibp
054500040910     c                   else
054600040910     c                   Eval      %Subst(comman:30:10) = wlib
054700040910     c                   endif
054800040910
054900040823     c                   Eval      comman =
055000040823     c                             %trim(comman) + '/FNARB01L)'
055100040823     c                   Call      'QCMDEXC'                            99
055200040823     c                   Parm                    comman
055300040823     c                   Parm                    lenght
055400040823     c                   If        *In99
055500040823     c                   seton                                        lr
055600040823     c                   Else
055700040823     c                   Open      Fnarb01l
055800040823     c                   EndIf
055900040823      *
056000931115     C*
056100950717     C                   EXSR      SUBDAT                                       *RECUPERA DATA E ORA
056200080925     c* carico i tipi bolla RECUPERO per evitare di salvarli nel TITASS0F
056300081103     c                   clear                   conta
056400080926     c                   open      tabel00f
056500080925     c     keytab1       setll     tabel00f
056600080925     c                   do        *hival
056700080925     c     keytab1       reade(n)  tabel00f
056800080925     c                   if        %eof(tabel00f)
056900080925     c                   leave
057000080925     c                   end
057100080925     c                   movel(p)  tbluni        dstb
057200080925     c                   if        §TBRBL ='R'
057300080925     c                   add       1             conta             2 0
057400080925     c                   movel     tblkey        tbl(conta)
057500080925     c                   end
057600080925     c                   enddo
057700080926     c                   close     tabel00f
057800000000     C*
057900000000     C                   ENDSR
058000940912     O*------------------------------------------------------------------------*
058100940905     OPRTF198   E            TESTA            02
058200950309     O                                            8 'TNSB38R '
058300931105     O                       RSUT                45
058400990802     O                                           82 'TRASF. TITASP -> SALVAT.'
058500931115     O                                           90 'DAL:'
058600950309     O                       PARDTI              99
058700931115     O                                          110 'AL:'
058800950309     O                       PARDTF             119
058900020220     O                                          128 '/PT'
059000020220     O                       PARDFP             132
059100931105     O                                          150 'Data:'
059200950717     O                       DSDAT6             159 '  /  /  '
059300931105     O                                          167 'Ora:'
059400950717     O                       DTORA6             178 '  .  .  '
059500931105     O                                          190 'Pagina:'
059600931105     O                       PAGE          Z    198
059700931105     O          E            TESTA       1  1
059800931105     O                       TES(2)              66
059900931105     O                       TES(3)             132
060000931105     O                       TES(4)             198
060100931105     O          E            TESTA          1
060200931105     O                       TES(1)              66
060300931105     O                       TES(1)             132
060400931105     O                       TES(1)             198
060500001019     O*------------------------------------
060600001019     O          E            ERRDCM         1
060700001019     O                       TASAAS            +  5
060800001019     O                       TASLNP            +  1
060900001019     O                       TASNRS            +  1
061000110302     O                       TASNSP        2   +  1
061100001019     O                       TASTBL            +  1
061200001019     O                                         +  1 'BOLLA NON CONS.    '
061300110302     o               20                          50 ' '
061400110302     O               20                        +  0 'presente in FNARB '
061500110302     O               20                        +  0 'con blocco cons."B"'
061600110302     O               20                        +  1 'RIMPALLO'
061700110302     o               21                          50 ' '
061800110302     O               21                        +  0 'bolla linea di arrivo 101'
061900110302     o               22                          50 ' '
062000110302     O               22                        +  0 'bolla legata a filgi con '
062100110302     O               22                        +  0 'lna 101'
062200110302     o               23                          50 ' '
062300110302     O               23                        +  0 'bolla legata a figlia '
062400110302     O               23                        +  0 'con blocco cons."B"'
062500110302     O               23                        +  1 'RIMPALLO'
062600931105     O*------------------------------------
062700950307     O          E            ERRTAS         1
062800950307     O                       TASAAS            +  5
062900950307     O                       TASLNP            +  1
063000950307     O                       TASNRS            +  1
063100110302     O                       TASNSP        2   +  1
063200950307     O                       TASTBL            +  1
063300990802     O                                         +  1 'ERR. WRITE TITAS   '
063400000404     O*------------------------------------
063500000404     O          E            ERRGCP         1
063600000404     O                       GCPAAS            +  5
063700000404     O                       GCPLNP            +  1
063800000404     O                       GCPNRS            +  1
063900000404     O                       GCPNSP            +  1
064000050301     O                                         +  1 'ERR. WRITE TIGCP   '
064100050303     O          E            ERRGCP1        1
064200050303     O                       GCPAAS            +  5
064300050303     O                       GCPLNP            +  1
064400050303     O                       GCPNRS            +  1
064500110302     O                       GCPNSP        2   +  1
064600050303     O                                         +  1 'GIACENZA ANCORA IN FILIALE'
064700000404     O*------------------------------------
064800000404     O*------------------------------------
064900000404     O          E            NOCGCP         1
065000000404     O                       GCPAAS            +  5
065100000404     O                       GCPLNP            +  1
065200000404     O                       GCPNRS            +  1
065300110302     O                       GCPNSP        2   +  1
065400050301     O                                         +  1 'BOLLA NON CANC:TIGCP'
065500040922     O*------------------------------------
065600040922     O          E            ERRblp         1
065700040922     O                       blpAAS            +  5
065800040922     O                       blpLNP            +  1
065900040922     O                       blpNRS            +  1
066000110302     O                       blpNSP        2   +  1
066100110224     O                       blpCBO            +  1
066200040922     O                                         +  1 'ERR. Trovato reK su FNBLP'
066300040922     O*------------------------------------
066400040922     O          E            ERRarb         1
066500040922     O                       arbAAS            +  5
066600040922     O                       arbLNP            +  1
066700040922     O                       arbNRS            +  1
066800110302     O                       arbNSP        2   +  1
066900110224     O                       blpCBO            +  1
067000040922     O                                         +  1 'ERR. Trovato reK su FNARB'
067100040922     O*------------------------------------
067200950308     O          E            STAFIN         1
067300990802     O                                           30 'N.REC.TRASF. TITAS'
067400950308     O                       NUMTAS        2     44
067500950308     O          E            STAFIN         1
067600990802     O                                           30 'N.REC.TRASF. TITA7'
067700950308     O                       NUMTA7        2     44
067800990802     O          E            STAFIN         1
067900990802     O                                           30 'N.REC.TRASF. TITAA'
068000990802     O                       NUMTAA        2     44
068100990802     O          E            STAFIN         1
068200990802     O                                           30 'N.REC.TRASF. TITAT'
068300990802     O                       NUMTAT        2     44
068400990802     O          E            STAFIN         1
068500990802     O                                           30 'N.REC.TRASF. TITA4'
068600990802     O                       NUMTA4        2     44
068700010903     O          E            STAFIN         1
068800010903     O                                           30 'N.REC.TRASF. TITAH'
068900010903     O                       NUMTAH        2     44
069000950308     O          E            STAFIN         1
069100950308     O                                           30 'N.REC.TRASF. ECCET'
069200950308     O                       NUMCET        2     44
069300950308     O          E            STAFIN         1
069400950308     O                                           30 'N.REC.TRASF. ECCED'
069500950308     O                       NUMCED        2     44
069600980325     O          E            STAFIN         1
069700980325     O                                           30 'N.REC.TRASF. ECFIM'
069800980325     O                       NUMFIM        2     44
069900970903     O          E            STAFIN         1
070000970903     O                                           30 'N.REC.TRASF. ECECT'
070100970903     O                       NUMECT        2     44
070200970903     O          E            STAFIN         1
070300970903     O                                           30 'N.REC.TRASF. ECECD'
070400970903     O                       NUMECD        2     44
070500000327     O          E            STAFIN         1
070600000327     O                                           30 'N.REC.TRASF. FNEVB'
070700000327     O                       NUMEVB        2     44
070800000404     O          E            STAFIN         1
070900050301     O                                           30 'N.REC.TRASF. TIGCP'
071000000404     O                       NUMGCP        2     44
071100000404     O          E            STAFIN         1
071200050301     O                                           30 'N.REC.TRASF. TIGAP'
071300000404     O                       NUMGAP        2     44
071400000404     O          E            STAFIN         1
071500050301     O                                           30 'N.REC.TRASF. TIGNP'
071600000404     O                       NUMGNP        2     44
071700040310     O          E            STAFIN         1
071800040310     O                                           30 'N.REC. BOLLE FIAR5'
071900040310     O                       NUMAR5        2     44
072000060919     O          E            STAFIN         1
072100060919     O                                           30 'N.REC. BOLLE TITAI'
072200060919     O                       NUMTAI        2     44
072300081103     O          E            STAFIN         1
072400081103     O                                           30 'N.REC. BOLLE FIRAB'
072500081103     O                       NUMRAB        2     44
072600061122     O          E            STAFIN         1
072700061122     O                                           30 'N.BOLLE IN STORICO'
072800061122     O                       NUMSTO        2     44
072900001019     O          E            STAFIN         1
073000001020     O                                           30 'N.REC. BOLLE SCART'
073100001019     O                       NUMNON        2     44
073200021021     O          E            STAFIN         1
073300021021     O                                           30 'DI CUI:'
073400021021     O          E            STAFIN         1
073500021021     O                                           30 'X GIACENZA APERTA '
073600021021     O                       CNTGCP        2     44
073700021021     O          E            STAFIN         1
073800021021     O                                           30 'X DANNO PENDENTE  '
073900021021     O                       CNTDCT        2     44
074000021021     O          E            STAFIN         1
074100021021     O                                           30 'X C/A ATTIVO      '
074200021021     O                       CNTCSB        2     44
074300040922     O          E            STAFIN         1
074400040922     O                                           30 'N.REK.Trovati FNBLP'
074500040922     O                       NUMblp        2     44
074600040922     O          E            STAFIN         1
074700040922     O                                           30 'N.REK.Trovati FNARB'
074800040922     O                       NUMarb        2     44
074900060626     O          E            STAFIN         1
075000060626     O                                           +0 'Chiusuara pgm: FINTAS'
075100060626     O                       fintas              +1
075200060626     O                       tasdsp              +1
075300060626     O                       dsdatf              +1
075400060626     O               98                          +1 'SHTDN'
075500060626     O                                           +1 'FINL00'
075600060626     O                       finl00              +1
075700950308     O          E            STAFIN           60
075800931115     O                                           76 '*** FINE STAMPA ***'
075900931115     O                                          150 'Data:'
076000950717     O                       DSDAT6             159 '  /  /  '
076100990802     O                                          167 'Ora:'
076200950717     O                       DTORA6             178 '  :  :  '
076300970903     O*------------------------------------------------------------------------*
076400040823** cmd
076500040823OVRDBF FILE(        ) TOFILE(
076600931105**  TES - RIGHE IN STAMPA
076700931105------------------------------------------------------------------  1
076800110302       AA LNP SE N.SPED. TB     DESCRIZIONE ERRORE
076900950308E                                                                   3
077000931105                                                                    4
077100931105                                                                    5
077200931105                                                                    6
077300931105                                                                    7
077400931105                                                                    8
077500931105                                                                    9
077600931105                                                                   10
077700931105                                                                   11
077800931105                                                                   12
077900931105                                                                   13
078000931105                                                                   14
078100931105                                                                   15
