000100170321     H DFTACTGRP(*NO) BNDDIR('PJXBND':'YDR') ACTGRP('QILE')
000200860421     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300850705     F* OFN220R  *---------------------------------------------------*
000400850705     F*----------*                                                   *
000500850705     F*              - IMMISSIONE FATTURE FORNITORI -                *
000600850705     F*              --------------------------------                *
000700850705     F*                                                    AZ.: N220 *
000800850705     F*                                                              *
000900990614     F*         FILE : OFN220D  - tmmof00F - tmmof03L - tmfop01L     *
001000990614     F*                tmamp01L - tmmmp05L                           *
001100860528 RIC F*                TABEL00F                                      *
001200860528 ADB F*                BFTAB01L                                      *
001300850705     F*                                                              *
001400850705     F*         PGM. : ANU014R - RICERCA ALFABETICA FORNITORI        *
001500850705     F*                ANU015R - RICERCA ALFABETICA MATERIE PRIME    *
001600850705     F*                ANU016R - RICERCA CONDIZIONI DI PAGAMENTO     *
001700850705     F*                OFN221R - CONTABIZZAZIONE FATTURA             *
001800850705     F*                RICDATE - INVERSIONE E CONTROLLO DATE         *
001900850722     F*                RIC001R - RICERCA DESCRIZIONE DITTA           *
002000850705     F*                RIC002C - RICHIAMA RISORSE                    *
002100850705     F*                                                              *
002200860423     F*--------------------------------------------------------------*
002300860423     F* RIEPILOGO INDICATORI :                                       *
002400860423     F*                                                              *
002500860423     F*  01 = EMISSIONE PROMPT                                       *
002600860423     F*  02 = EMISSIONE RICERCA  BOLLE                               *
002700860423     F*  03 = EMISSIONE GESTIONE BOLLE SCELTE                        *
002800860423     F*  04 = RICHIESTA DI FORZATURA   X FORNITORE MATERIALE (N12)   *
002900860423     F*  05 = FORZATURA DI VALUTA      X   """       """"    (N12)   *
003000860423     F*  06 = FORZATURA DI PAGAMENTO   X   """       """"    (N12)   *
003100860530     F*  10 = CONTABILIZZAZIONE FATTURE                              *
003200860423     F*  12 = FATTURA DI SERVIZI                                     *
003300860506     F*  26 = FATTURA DI SERVIZI (NOTA DI ACCREDITO)                 *
003400860505     F*  80 = ERRORE GENERICO                                        *
003500860423     F*  99 = ERRORE DI LETTURA FILE/SUBFILE                         *
003600860423     F*  70 = PULIZIA   SUBFILE                                      *
003700860423     F*  71 = EMISSIONE SUBFILE                                      *
003800860423     F*  KJ = FINE LAVORO                                            *
003900860505     F*  KE = FORZATURA VALUTA                                       *
00400089092810   F*  KF = FORZATURA PAGAMENTO                                    *
004100860423     F*  KE = CONTABILIZZAZIONE FATTURA                              *
004200860423     F*  KG = RIPARTIZIONE GLOBALE ONERI                             *
004300860506     F*--------------------------------------------------------------*
004400860506     F* LE RIGHE DI ORDINE VENGONO COSI FLEGGATE :                   *
004500860506     F*  -E- = RIGHE ESTRATTE X FORNITORE DI PRODOTTO                *
004600860506     F*  - - = RIGHE ESTRATTE X FORNITORE DI SERVIZI                 *
004700860506     F*  -R- = RIGHE ESTRATTE CHE HANNO SUBITO UNA RIPARTIZIONE ONERI*
004800860506     F*                                                              *
004900860506     F* LE RIGHE DI ONERI  VENGONO COSI FLEGGATE :                   *
005000860506     F*  -O- = RIGHE DI ONERI AGGIUNTI NON RIPARTITI                 *
005100860506     F*  - - = RIGHE DI ONERI AGGIUNTI     RIPARTITI                 *
005200860506     F*                                                              *
005300850705     F*--------------------------------------------------------------*
005400990614     Ftii410D   CF   E             WORKSTN
005500000000     F                                     SFILE(SFL1:REC1)
005600000000     F                                     SFILE(SFL2:REC2)
005700990614     Ftmmof00F  UF A E             DISK
005800990614     F                                     RENAME(tmmof000:ORDFIS)
005900990614     Ftmmof03L  IF   E           K DISK
006000990614     F                                     RENAME(tmmof000:ORDBOL)
006100000000     F                                     INFDS(INF3)
006200990614     Ftmfop01L  IF   E           K DISK
006300990614     Ftmamp01L  IF   E           K DISK
006400860528RIC  F*****************************************************************
006500860528RIC  FTABEL00F  IF   E           K DISK
006600860528     F*
006700860528ADB  F*****************************************************************
006800990614     Ftmmmp05L  UF   E           K DISK
006900860424     FOFPRTSTD  O    F  132        PRINTER OFLIND(*INOF)
007000860424     F                                     USROPN
007100890928     D ERR             S             40    DIM(30) CTDATA PERRCD(1)             * SEGNALAZIONI
007200860421     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              * TESTATA 1
007300860421     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              * TESTATA 2
007400860421     D SK3             S             66    DIM(2) CTDATA PERRCD(1)              * TESTATA 3
007500860421     D SK4             S             66    DIM(2) CTDATA PERRCD(1)              * TESTATA 4
007600860421     D SK5             S             66    DIM(2) CTDATA PERRCD(1)              * TESTATA 5
007700860421     D SK6             S             66    DIM(2) CTDATA PERRCD(1)              * TESTATA 6
007800860429     D SK7             S             66    DIM(2) CTDATA PERRCD(1)              * TESTATA 7
007900860429     D SK8             S             66    DIM(2) CTDATA PERRCD(1)              * TESTATA 8
008000860429     D SK9             S             66    DIM(2) CTDATA PERRCD(1)              * TESTATA 9
008100860421     D*--------------------------------------------------*
008200000000     D                 DS
008300860528     D*-----SCOMPOSIZIONE CODICE / VERSIONE
008400871008     D  MOFMAT                 1     12
008500871008     D  CODBAR                 1     12
008600000000     D INF3            DS           512
008700000000     D*--- RELREC LOGICO
008800000000     D  NREL3                397    400B 0
008900860528RIC  D PAGAM           DS
009000860528RIC  D*--- KEY PAGAMENTI PER TABEL00F X PAGAMENTI :
009100860528RIC  D  PAGVID                 1      3
009200860528RIC  D  LINGUA                 4      4
009300860528RIC  D  BLANCO                 5      8
009400860528RIC  D  KEYTBL                 1      8
009500000000     D PAR221          DS
009600000000     D*--- PARAMETRI PASSAGGIO X CONTABILIZZ.FATTURA
009700860423     D  NRC221                 1      4  0
009800860423     D  OKO221                 5      6
009900860423     D  CFO221                 7     12
010000990614     D  FOR221                13     20
010100990614     D  DES221                21     49
010200860423     D  CPG221                50     52
010300860423     D  VAL221                53     56
010400860423     D  CMB221                57     63  2
010500860423     D  IMP221                64     75  2
010600860423     D  NFT221                76     82  0
010700990614     D  DFT221                83     90  0
010800990614     D  TIP221                91     91
010900990614     D  NRG221                92     98  0
011000990614     D  DRG221                99    106  0
011100990614     D  CAU221               107    110
011200860528     D OFNDT1          DS
011300860528     D*-------------- DATA AREA ORDINI FORNITORI :
011400860528     D  F01DT1                 1    100
011500860528     D  FLGADB               101    101
011600860528     D  F02DT1               102    256
011700860528     D  F03DT1               257    512
011800890704     D OFNNPR          DS
011900890704     D*--- DATA AREA NUMERO PROTOCOLLO IVA FORNITORI
012000890704     D  NPRNRG                 1      6  0
012100890704     D  NPRDRG                 7     12  0
012200860528     D KPJBA         E DS
012201171019     D IMOFDS00      E DS
012300860528     D*-------------- PASSAGGIO PARAMETRI :
012400860528     D  MSGERR               453    502
012500990614     D soc001        E DS                  EXTNAME(xsoc001ds)
012600990614     D xsocds          DS          1000
012700990614     d dataiso         s               d   datfmt(*iso)
012800990614     d dataeur         s               d   datfmt(*eur)
012900170321     d primogiro       s              1
013000990614      *
013100170321     D EsitoA          S              1
013200990614     D Datigest        S              1A
013300990614     D NoctrlScos      S              1A
013400990614     D Gesmsg          S              1
013500170321      *__________________________________________________________
013600170321     D Skmessaggi      s            100a   DIM(255) varying
013800170321     D idrigaere       s             10I 0 inz
013900170321     D nrMessaggi      s             10I 0 inz
014000170321     D idMessaggio     s              7    dim(255)
014100170321     D tipoMessaggio   s              1    dim(255)
014200170321     D idcampo         s             10    dim(255)
014300170321     D testomessaggio  s            255a   DIM(255) varying
014600170321     d ydrereds      e ds                  extname(ydrere00f)
014700170321     d                                     QUALIFIED INZ(*EXTDFT)
014701171019     D pgmname         s             10    INZ('TII411R')
014800170321     D*  Prototipazione delle procedure
014900170321      /COPY GAITRASRC/SRCCONST,YDRERE
015000170321      /COPY GAITRASRC/SRCPROTOPR,YDRERE
015100990614     D/COPY *LIBL/SRCCPY,PJX002PR
015101171019     d
015102171019     d ydrtfrjob       pr                  extpgm('YDRTFRJOB')
015103171019     d  kpjba                              likeds(kpjba)
015104171019     d  PGMNAME                            LIKE(PGMNAME)
015105171019     d
015200000000     C*----------------------------------------------------*
015300000000     C     *ENTRY        PLIST                                                  *
015400000000     C                   PARM                    KPJBA                          *
015500860421     C*                                                    *
015600860421     C     *LIKE         DEFINE    KPJBU         KPJBUW                         *
015700860528     C*----------- DATA AREA ORDINI FORNIT.:               *
015800890704     C     *DTAARA       DEFINE                  OFNNPR                         *
015900860528     C     *DTAARA       DEFINE                  OFNDT1                         *
016000890704     C                   Z-ADD     0             NPRNRG
016100890704     C                   Z-ADD     0             NPRDRG
016200860528     C     SUDT1         TAG                                                    *
016300860528     C     *LOCK         IN        OFNDT1                               07      *
016400860528     C   07              GOTO      SUDT1                                        *
016500860528     C                   OUT       OFNDT1                                       *
016600990614     C*--- FILE MATERIE PRIME "tmamp01L" :                 *
016700000000     C     KEYART        KLIST                                                  *
016800860421     C                   KFLD                    CODMAT                         *
016900860528     C******************************************************
017000860528RIC  C*--- FILE ANAGRAFICO TABELLE X PAGAMENTO :           *
017100860528RIC  C     KEYPAG        KLIST                                                  *
017200990614RIC  C                   KFLD                    CODUTE            1 0          *
017300860528RIC  C                   KFLD                    TIPTAB            2            *
017400860528RIC  C                   KFLD                    KEYTBL                         *
017500891212     C                   MOVEL     'VFAT'        CAU221
017600990616RIC  C                   MOVE      '1'           LINGUA                         *
017700990614     C                   z-add     1             Codute
017800000000     C*--- RICERCA DITTA ----------------------------------*
017900990614     C                   MOVEL     'SOC001'      TIPXSC
018000990614     C                   MOVEL     *blank        SOCXSC
018100990614     C                   EXSR      REPSOC
018200990614     C     RTNXSC        IFNE      '1'
018300990614     C                   MOVEL     XSOCDS        SOC001
018400990614     c                   end
018500990614     c                   time                    como14           14 0
018600990614     c                   move      como14        woggi             8 0
018700990614     c                   move      woggi         dataeur
018800990614     c                   move      dataeur       dataiso
018900990614     c                   move      dataiso       woggig            8 0
019000000000     C*---                                                 *
019100860423     C                   MOVE      *ZEROS        PAR221                         *
019200990614     C                   MOVE      '?       '    CODFOR                         *
019300860421     C                   MOVE      *BLANKS       NOMFOR                         *
019400860421     C                   MOVE      *BLANKS       FORSER                         *
019500860421     C                   MOVE      *BLANKS       NOMSER                         *
019600860424     C                   MOVE      '9999999'     NUMBOL                         *
019700860505     C                   MOVEL     ERR(25)       DESMAT                         *
019800860423     C                   MOVE      *BLANKS       ERRORE                         *
019900860421     C                   Z-ADD     0             SC1NRG                         *
020000990614     C                   Z-ADD     woggi         SC1DRG                         *
020100860530     C*-------------                                       *
020200860530     C     TAG009        TAG                                                    *
020300990616     C                   MOVE      *all'9'       CODMAT                         *
020400860424     C*-------------                                       *
020500860424     C     TAG010        TAG                                                    *
020600860505     C                   Z-ADD     0             SC1NFT                         *
020700860505     C                   Z-ADD     0             SC1DFT                         *
020800860505     C                   MOVE      *BLANKS       SC1CPG                         *
020900860505     C                   Z-ADD     0             SC1IMP                         *
021000860424     C                   MOVE      *BLANKS       SC1VAL                         *
021100860505     C                   Z-ADD     0             SC1CMB                         *
021200860423     C*----------- EMISSIONE TESTATA -1- ------------------*
021300000000     C                   SETOFF                                       0203      *
021400000000     C                   SETON                                        01        *
021500000000     C                   WRITE     OFN220T1                                     *
021600000000     C*--- EMISSIONE PROMPT RICHIESTA FORNITORE :          *
021700860505     C     TAG011        TAG                                                    *
021800000000     C                   EXFMT     OFN220PT                                     *
021900860423     C                   MOVEA     *ZEROS        *IN(2)                         *
022000860505     C                   MOVE      *BLANKS       ERRORE                         *
022100990614     C   Kc              GOTO      FINE                                         *
022200860421     C*--- RICERCA ALFABETICA FORNITORE MATERIALE ---------*
022300990614     C     CODFOR        IFEQ      '?       '                                   *
022400990614     C                   CALL      'TII103R'                                    *
022500860421     C                   PARM                    CODFOR                         *
022600860421     C                   PARM                    NOMFOR                         *
022700860505     C                   SETON                                            81    *
022800860505     C                   GOTO      TAG011                                       *
022900860421     C                   END                                                    *
023000860421     C*--- RICERCA PER CODICE FORNITORE MATERIALE ---------*
023100860421     C                   TESTN                   CODFOR               46        *
023200860505     C     *IN(46)       CABEQ     '0'           TAG011                   81    *
023300990614     C     CODFOR        CHAIN     tmfop01L                           81        *
023400860505     C   81              GOTO      TAG020                                       *
023500860421     C                   MOVEL     FOPRSI        NOMFOR                         *
023600860421     C                   MOVEL     FOPRSI        NOMFO1                         *
023700860421     C*--- RICERCA ALFABETICA FORNITORE SERVIZI -----------*
023800990614     C     FORSER        IFNE      '        '                                   *
023900990614     C     FORSER        IFEQ      '?       '                                   *
024000990614     C                   CALL      'TII103R'                                    *
024100860421     C                   PARM                    FORSER                         *
024200860421     C                   PARM                    NOMSER                         *
024300860505     C                   SETON                                            82    *
024400860505     C                   GOTO      TAG011                                       *
024500860421     C                   END                                                    *
024600860421     C*--- RICERCA PER CODICE FORNITORE SERVIZI -----------*
024700860421     C                   TESTN                   FORSER               46        *
024800860505     C     *IN(46)       CABEQ     '0'           TAG011                   82    *
024900990614     C     FORSER        CHAIN     tmfop01L                           82        *
025000860505     C   82              GOTO      TAG020                                       *
025100860421     C                   MOVEL     FOPRSI        NOMSER                         *
025200860421     C                   MOVEL     FOPRSI        NOMSE1                         *
025300860424     C                   SETON                                            12    *
025400860424     C                   ELSE                                                   *
025500860424     C                   MOVEL     *BLANKS       NOMSER                         *
025600860424     C                   MOVEL     *BLANKS       NOMSE1                         *
025700860424     C                   END                                                    *
025800860424     C*-------------                                       *
025900860505     C     SC1VAL        IFEQ      *BLANKS                                      *
026000860505     C                   MOVEL     FOPVAL        SC1VAL                         *COD.VALUTA
026100860505     C                   END                                                    *
026200860424     C     SC1VAL        IFEQ      *BLANKS                                      *
026300860424     C                   Z-ADD     1             SC1CMB                         *
026400860424     C                   END                                                    *
026500000000     C*--- CONTROLLO NUMERO BOLLA -------------------------*
026600860505     C     NUMBOL        CABLE     0             TAG020                   83    *
026700860421     C*--- CONTROLLO PRODOTTO -----------------------------*
026800990614     C                   MOVEL     *all'9'       NOVE             12
026900871008     C                   MOVEL     '?'           INTERR           12
027000870105     C     CODMAT        IFNE      NOVE                                         *
027100870105     C     CODMAT        IFEQ      INTERR                                       *
027200860421     C                   MOVE      *BLANKS       CODMAT                         *
027300990614     C                   CALL      'TII104R'                                    *
027400860421     C                   PARM                    CODMAT                         *
027500860421     C                   PARM                    DESMAT                         *
027600860505     C                   SETON                                            84    *
027700860505     C                   GOTO      TAG011                                       *
027800000000     C                   END                                                    *
027900860421     C*----------                                          *
028000990614     C     KEYART        CHAIN     tmamp01L                           84        *
028100860505     C   84              GOTO      TAG020                                       *
028200860421     C                   MOVEL     AMPDMT        DESMAT                         *
028300000000     C                   END                                                    *
028400860505     C*--- CONTROLLO FATTURA ------------------------------*
028500990616     C     *eur          test(d)                 sc1dft                 66      *
028600990616     c   66              seton                                          85
028700990616     c                   if        *in66
028800170321     C   85*eur          test(d)                 sc1dft                 85      *
028900170321     c  n85*eur          move      sc1dft        dataeur
029000990616     c  n85              move      dataeur       sc1dft
029100990616     c                   end
029200860505     C  N85SC1NFT        COMP      0                                    8585    *NUM. FATTURA
029300860505     C   85              GOTO      TAG020                                       *
029400860505     C*--- CONTROLLO PAGAMENTO ----------------------------*
029500860505     C     SC1CPG        IFEQ      '?  '                                        *PAGAMENTO
029600860505     C                   MOVE      *ZEROS        SC1CPG                         *
029700860505     C                   MOVE      *BLANKS       DESL30           30            *
029800990614     C                   CALL      'TII105R'                                    *
029900860505     C                   PARM                    SC1CPG                         *
030000860505     C                   PARM                    DESL30                         *
030100860505     C                   SETON                                            86    *
030200860505     C                   GOTO      TAG011                                       *
030300860505     C                   END                                                    *
030400860528     C******************************************************
030500860528 RIC C                   MOVE      'FA'          TIPTAB                         *
030600860528 RIC C                   MOVE      SC1CPG        PAGVID                         *
030700860528 RIC C     KEYPAG        CHAIN     TABEL00F                           86        *
030800860528     C******************************************************
030900860505     C   86              GOTO      TAG020                                       *
031000860505     C*--- CONTROLLO IMPORTO FATTURA ----------------------*
031100860505     C     SC1IMP        CABEQ     0             TAG020                   87    *
031200860505     C*--- CONTROLLO VALUTA/CAMBIO ------------------------*
031300860505     C     SC1VAL        IFEQ      *BLANKS                                      *
031400860505     C                   Z-ADD     1             SC1CMB                         *
031500860505     C                   ELSE                                                   *
031600860528RIC  C******************************************************
031700990616RIC  C                   MOVEL     SC1VAL        VALUTA            4            *
031800990614RIC  C     valuta        IFEQ      xscdiv                                       *
031900990614RIC  C                   Z-ADD     1             sc1CMB                         *
032000990614RIC  C                   ELSE                                                   *
032100990614     C                   move      valuta        divisa0202
032200990614     C                   z-add     0             Erro              1 0
032300990614     C                   Eval      Erro = X0201rpdvs(divisa0202:dtrif0202 :
032400990614     C                                           ANDVS00F:%size(ANDVS00F) :
032500990614     C                                           ANDVX00F:%size(ANDVX00F))
032600990614     C                   if        Erro<> 1 and Erro<> 3
032700990614     C                   reset                   PJX00202DS
032800990614     C                   movel     valuta        divisa0202
032900990614     C                   movel     xscdiv        divmc0202
033000990614     C                   z-add     1000          impord0202
033100990614     C                   movel     *on           Gesmsg
033200170321     C                   movel     *blanks       EsitoA
033300990614     C                   movel     *on           Noctrlscos
033400990614     C                   callp     x0202testc(PJX00202DS:
033500990614     C                                  %size(PJX00202DS):
033600170321     C                                  *off : EsitoA : Noctrlscos :
033700990614     C                                  Datigest :
033800990614     C                                  %size(ANDVS00F):
033900990614     C                                  %size(ANDVX00F):
034000990614     C                                  ANDVS00F : ANDVX00F )
034100170321     C                   If        EsitoA = '0'
034200990614     c                   z-add     cambio0202    sc1cmb
034300990614     c                   end
034400990614     c                   end
034500990614     c                   END
034600860528RIC  C******************************************************
034700860528     C*----------------------------------------------------*
034800860505     C  N88SC1CMB        COMP      0                                    8888    *
034900860505     C   88              GOTO      TAG020                                       *
035000860505     C                   END                                                    *
035100860505     C*--- CONTROLLO DATA/NUMERO REGISTRAZIONE ------------*
035200890704     C                   EXSR      RICNPR
035300890704     C** RICERCA NUMERO E DATA PROT.IVA
035400890704     C     SC1NRG        IFEQ      0
035500890704     C                   Z-ADD     NPRNRG        SC1NRG
035600890704     C                   END
035700890704     C** SE N.RO REG.NE = 0 IMPOSTO DALLA DATA AREA
035800890704     C     SC1NRG        COMP      0                                    8989    *
035900890704     C   89              GOTO      TAG020                                       *
036000890704     C**
036100990618     C     *eur          test(d)                 sc1drg                 66      *
036200990618     c   66              seton                                          89
036300990618     c                   if        *in66
036400170321     C   89*eur          test(d)                 sc1drg                 89      *
036500170321     c  n89*eur          move      sc1drg        dataeur
036600990618     c  n89              move      dataeur       sc1drg
036700990618     c                   end
036800990618     C   89              GOTO      TAG020                                       *
036900990618     c                   move      sc1drg        dataeur
037000990618     c                   move      dataeur       dataiso
037100990618     C                   move      dataiso       AMGSC1            8 0
037200990618     C     *ymd          move      nprdrg        dataiso
037300990618     C                   move      dataiso       nprdrg8           8 0
037400990618     C     NPRDRG8       COMP      AMGSC1                             89
037500990618     C   89              GOTO      TAG020
037600890704     C** CONTROLLO DATA FATTURA VIDEO CON DATA AREA PROTOCOLLO
037700860505     C*----------------------------------------------------*
037800000000     C*--- CONTROLLO PRESENZA ERRORI SU PROMPT ------------*
037900000000     C     TAG020        TAG                                                    *
038000860505     C     81            DO        91            X                 3 0          *
038100000000     C     *IN(X)        COMP      '1'                                    80    *
038200000000     C  N80              END                                                    *
038300000000     C   80              DO                                                     * ERRORI
038400860505     C     X             SUB       80            E                 2 0          *
038500860505     C                   MOVEL     ERR(E)        ERRORE                         *
038600860505     C                   GOTO      TAG011                                       *
038700000000     C                   END                                                    *
038800860423     C*---------- LETTURA RIGHE ORDINE BB/FA --------------*
038900860423     C                   Z-ADD     0             REC1              4 0          *
039000000000     C                   Z-ADD     0             RECT1             4 0          *
039100860421     C                   Z-ADD     1             CURSOR                         *
039200860424     C                   Z-ADD     0             SF1VID                         *
039300000000     C                   Z-ADD     0             SF1REC                         *
039400860423     C                   Z-ADD     0             SF1NRE            4 0          *
039500000000     C                   SETON                                            70    *
039600000000     C                   WRITE     SFL1CT                                       *
039700860424     C                   MOVEA     *ZEROS        *IN(13)                        *
039800860505     C                   MOVE      *BLANKS       ERRORE                         *
039900860421     C*---------- RICERCA RIGHE BOLLETTATE ----------------*
040000990614     C     CODFOR        SETLL     tmmof03L                           90        *
040100860505     C   90              GOTO      TAG020                                       * NESSUN ORDINE
040200860421     C*----------                                          *
040300860421     C     RICBOL        TAG                                                    *
040400990614     C     CODFOR        READE     tmmof03L                               46    *
040500000000     C   46              GOTO      TAG040                                       *
040600860421     C*--------- ESCLUSIONE TIPI RIGA :                    *
040700860421     C     MOFTRG        CABEQ     '3'           RICBOL                         *ORD. PRODUZ.
040800860421     C     MOFTRG        CABEQ     '6'           RICBOL                         *COMM.FORNIT.
040900860421     C     MOFTRG        CABEQ     '7'           RICBOL                         *COMM.INTERNO
041000860421     C     MOFTRG        CABEQ     '8'           RICBOL                         *COMM.MAGAZZ.
041100860421     C     MOFTRG        CABEQ     '9'           RICBOL                         *COMM.GENERICO
041200860423     C*---------- TEST FORNIT. SERVIZI/PRODOTTO -----------*
041300860424     C   12MOFFAS        CABNE     'FA'          RICBOL                         *FORNIT.SERVIZI
041400860429     C   12MOFTRG        CABEQ     'D'           RICBOL                         *
041500860429     C   12MOFTRG        CABEQ     'I'           RICBOL                         *
041600860429     C   12MOFTRG        CABEQ     'S'           RICBOL                         *
041700860424     C  N12MOFFAS        CABNE     'BB'          RICBOL                         *FORNIT.PRODOTT
041800860423     C*--------- TEST INCLUSIONI/OMISSIONI ----------------*
041900860421     C     NUMBOL        IFNE      9999999                                      *
042000860421     C     MOFNBL        CABNE     NUMBOL        RICBOL                         *
042100860421     C                   END                                                    *
042200870107     C     CODMAT        IFNE      NOVE                                         *
042300860421     C     MOFMAT        CABNE     CODMAT        RICBOL                         *
042400000000     C                   END                                                    *
042500860423     C*--------- CARICAMENTO SUBFILE -1- ------------------*
042600860424     C     REC1          ADD       1             REC1                 71        *
042700860505     C                   Z-ADD     REC1          RECT1                    90    *
042800860505     C     MOFTRG        COMP      'D'                                    79    *
042900860505     C  N79MOFTRG        COMP      'I'                                    79    *
043000860505     C  N79MOFTRG        COMP      'S'                                    79    *
043100860421     C                   Z-ADD     NREL3         SF1REC                         * NRR FISICO
043200860421     C                   Z-ADD     REC1          SF1VID                         * NRR VIDEO
043300860424     C                   MOVE      MOFTRG        SF1TRG                         *
043400860612     C                   Z-ADD     SC1CMB        SF1CMB                         *
043500860424     C                   MOVE      MOFCOM        SF1COM                         *
043600990614     C                   z-add     MOFAIA        SF1AIA                         *
043700860424     C                   MOVE      MOFUMQ        SF1UMQ                         *
043800860424     C                   MOVE      MOFQTA        SF1QTA                         *
043900860424     C*----------- CALCOLO PREZZO SCONTATO                 *
044000860424     C     MOFPRZ        MULT      MOFSC1        SCONTO           12 2          *
044100860424     C     SCONTO        DIV       100           SCONTO                         *
044200860424     C     MOFPRZ        SUB       SCONTO        MOFPRZ                         *
044300860424     C     MOFPRZ        MULT      MOFSC2        SCONTO                         *
044400860424     C     SCONTO        DIV       100           SCONTO                         *
044500860424     C     MOFPRZ        SUB       SCONTO        MOFPRZ                         *
044600870202     C     MOFPRZ        MULT      MOFSC3        SCONTO           12 2          *
044700870202     C     SCONTO        DIV       100           SCONTO                         *
044800870202     C     MOFPRZ        SUB       SCONTO        MOFPRZ                         *
044900860424     C                   Z-ADD     MOFPRZ        SF1PRZ                         *
045000860424     C*------------                                        *
045100860424     C                   MOVE      *BLANK        SF1SCE                         *
045200860424     C                   MOVEL     CODBAR        SF1ART                         *
045300860424     C                   MOVEL     MOFDMT        SF1DMT                         *
045400860424     C                   Z-ADD     MOFNBL        SF1NBL                         *NUM. BOLLA
045500990614     C                   MOVEl     MOFCDC        SF1CDC
045600990614     C                   move      MOFDBL        DATAiso                        *
045700990614     C                   move      dataiso       dataeur                        *
045800990614     C                   move      DATAeur       SF1DBL                         *DATA BOLLA
045900860424     C                   Z-ADD     MOFNFT        SF1NFT                         *NUM. FATTURA
046000860505     C*----------- CONTROLLO VALUTA -----------------------*
046100860505     C                   MOVE      MOFVAL        SF1VAL                         *
046200860505     C     MOFVAL        IFNE      SC1VAL                                       *
046300860505     C                   SETON                                        77        *
046400860505     C  N12              SETON                                            05    *
046500860505     C                   END                                                    *
046600860505     C*----------- CONTROLLO PAGAMENTO FORN. MATERIALE ----*
046700860505     C                   MOVEL     MOFCPG        SF1CPG                         *
046800860505     C  N12SF1CPG        IFNE      SC1CPG                                       *
046900860505     C                   SETON                                        78  06    *
047000860505     C                   END                                                    *
047100860505     C*-----------                                         *
047200000000     C                   WRITE     SFL1                                         *
047300860505     C                   SETOFF                                       777879    *
047400860421     C                   GOTO      RICBOL                                       *
047500860505     C*----------- EMISSIONE TESTATA -2- ------------------*
047600860423     C     TAG040        TAG                                                    *
047700890706     C                   EXSR      AGGNPR
047800890706     C** AGGIORNO DATA AREA NUMERO E DATA REGISTRAZIONE
047900860423     C                   SETOFF                                       0103      *
048000860423     C                   SETON                                        02        *
048100860423     C                   WRITE     OFN220T1                                     *
048200860505     C*----------- OMOGENEITA DATI ------------------------*
048300860505     C   05                                                                     *
048400860505     COR 06              DO                                                     *
048500860505     C                   MOVEL     ERR(12)       ERRVAL                         *
048600860505     C                   MOVE      SC1VAL        ERRVAL                         *
048700860505     C                   MOVEL     ERR(13)       ERRPAG                         *
048800860505     C                   MOVE      SC1CPG        ERRPAG                         *
048900860505     C                   MOVEL     ERR(14)       ERRORE                         *
049000860505     C                   SETON                                            80    *
049100860505     C                   END                                                    *
049200860423     C*--- EMISSIONE SUBFILE 1 / E RIGA FONDO -------------*
049300000000     C     TAG060        TAG                                                    *
049400000000     C     CURSOR        DIV       14            CURSOR                         * POSIZ.CURSORE
049500860423     C                   MVR                     RESTO             5 046        *
049600000000     C     CURSOR        MULT      14            CURSOR                         *
049700860423     C   46CURSOR        ADD       1             CURSOR                         *
049800000000     C                   EXFMT     SFL1CT                                       * SFL1CT
049900000000     C                   MOVEA     *ZEROS        *IN(75)                        *
050000860505     C                   MOVE      *BLANKS       ERRORE                         *
050100860423     C*---------- FINE LAVORO :                            *
050200990616     c   kc              goto      tag010
050300000000     C*--- LEGGO RIGHE RICHIAMATE -------------------------*
050400860423     C                   SETON                                        46        *
050500860423     C     1             DO        RECT1         REC1                           *
050600860423     C     REC1          CHAIN     SFL1                               99        * AIUTO !
050700000000     C   99              GOTO      TAG010                                       *
050800860424     C     SF1SCE        CABEQ     *BLANKS       TAG090                         *
050900860423     C*----------- CONTROLLO VALUTA -----------------------*
051000860505     C   KE              MOVE      SC1VAL        SF1VAL                         *
051100990614     C   KE              z-add     SC1CMB        SF1CMB                         *
051200860505     C   KE              Z-ADD     0             SF1PRZ                         *
051300860423     C     SF1VAL        IFNE      SC1VAL                                       *
051400860505     C                   SETON                                        77  80    *
051500860505     C                   MOVEL     ERR(14)       ERRORE                         *
051600000000     C                   Z-ADD     SF1VID        CURSOR                         * POSIZ.CURSORE
051700860505     C                   END                                                    *
051800860423     C*----------- CONTROLLO PAGAMENTO FORN. MATERIALE ----*
051900860505     C   KF              MOVE      SC1CPG        SF1CPG                         *
052000860423     C  N12SF1CPG        IFNE      SC1CPG                                       *
052100860505     C                   SETON                                        78  80    *
052200860505     C                   MOVEL     ERR(14)       ERRORE                         *
052300000000     C                   Z-ADD     SF1VID        CURSOR                         * POSIZ.CURSORE
052400860505     C                   END                                                    *
052500860505     C*---------- AGGIORNO SUBFILE :                       *
052600000000     C                   UPDATE    SFL1                                         *
052700860505     C                   SETOFF                                       467778    *
052800000000     C     TAG090        TAG                                                    *
052900000000     C                   END                                                    *
053000860421     C*--- NON ESTRATTA NESSUNA RIGA ----------------------*
053100860505     C   46              MOVEL     ERR(15)       ERRORE                         *
053200860505     C   46              SETON                                        80        *
053300860505     C   80              GOTO      TAG060                                       *
053400860505     C                   SETOFF                                       0506      *
053500000000     C*--- PULIZIA SUBFILE "2" ----------------------------*
053600860423     C                   Z-ADD     0             REC2              4 0          *
053700860423     C                   Z-ADD     0             RECT2             4 0          *
053800860423     C                   Z-ADD     1             CURSOR                         * POSIZ.CURSORE
053900860429     C                   Z-ADD     0             SC2TOT                         *
054000000000     C                   SETON                                            70    *
054100000000     C                   WRITE     SFL2CT                                       *
054200860423     C                   MOVEA     *ZEROS        *IN(40)                        *
054300860424     C                   MOVE      *BLANKS       ERRORE                         *
054400000000     C*--- LEGGO RIGHE SCELTE SU SFL1 E CARICO SFL2 -------*
054500000000     C     1             DO        RECT1         REC1                           *
054600000000     C     REC1          CHAIN     SFL1                               99        * AIUTO !
054700860421     C   99              GOTO      TAG010                                       *
054800000000     C*--- CONTROLLO SE LE RIGHE HANNO STESSA UNITA'MIS.---*
054900860424     C     SF1SCE        IFNE      *BLANKS                                      *
055000860424     C  N40              MOVE      SF1UMQ        SAVUMQ            3            *
055100860423     C  N40              SETON                                        40        *
055200860424     C     SF1UMQ        IFNE      SAVUMQ                                       *
055300000000     C                   SETON                                        41        *
055400000000     C                   END                                                    *
055500000000     C*---                                                 *
055600000000     C     REC2          ADD       1             REC2                 71        *
055700860424     C                   Z-ADD     REC2          RECT2                          *
055800860421     C                   Z-ADD     REC2          SF2VID                         *
055900860421     C                   Z-ADD     SF1REC        SF2REC                         *
056000860505     C                   MOVE      *BLANKS       SF2RIP                         *
056100860506     C  N12              MOVE      'E'           SF2RIP                         *
056200860424     C                   MOVE      SF1TRG        SF2TRG                         *
056300860424     C                   Z-ADD     SF1CMB        SF2CMB                         *
056400860424     C                   MOVEL     SF1COM        SF2COM                         *
056500860424     C                   Z-ADD     SF1AIA        SF2AIA                         *
056600860424     C                   MOVE      SF1UMQ        SF2UMQ                         *
056700860424     C                   MOVE      SF1QTA        SF2QTA                         *
056800860424     C                   Z-ADD     SF1PRZ        SF2PRZ                         *
056900161116     C  N12SF1PRZ        MULT      SF1QTA        PIPPO            15 5          *
057000860429     C  N12SF1TRG        IFEQ      'D'                                          *
057100860429     C     SC2TOT        SUB       PIPPO         SC2TOT                         *NEGATIVO
057200860429     C                   ELSE                                                   *
057300860429     C     SC2TOT        ADD       PIPPO         SC2TOT                         *
057400860429     C                   END                                                    *
057500860424     C                   MOVE      *BLANK        SF2SCE                         *
057600860424     C                   MOVE      SF1ART        SF2ART                         *
057700990616     C                   movel     SF1CDC        SF2CDC
057800860424     C                   MOVEL     SF1DMT        SF2DMT                         *
057900860424     C                   MOVEL     SF1DMT        SF2DES                         *
058000860424     C                   MOVEL     SF1VAL        SF2VAL                         *
058100860424     C                   MOVEL     SF1CPG        SF2CPG                         *
058200860424     C                   Z-ADD     0             SF2ONE                         *
058300860424     C                   Z-ADD     0             VALONE           11 2          *
058400860528     C*---------- RICERCA FATTORE CONVERSIONE -------------*
058500860528     C                   Z-ADD     1             SF2FCQ                         *
058600860528     C     SF1TRG        IFGE      '1'                                          *
058700860528     C     SF1TRG        IFLE      '3'                                          *
058800860528     C                   MOVEL     SF1ART        CODMAT                         *
058900990614     C     KEYART        CHAIN     tmamp01L                           99        *
059000860528     C  N99              Z-ADD     AMPFCQ        SF2FCQ                         *
059100860528     C                   END                                                    *
059200860528     C                   END                                                    *
059300860528     C*-----------                                         *
059400860528     C  N99              WRITE     SFL2                                         *
059500860423     C                   END                                                    *
059600860423     C                   END                                                    *
059700860423     C*----------- EMISSIONE TESTATA -3- ------------------*
059800860423     C                   SETOFF                                       0102      *
059900860423     C                   SETON                                        03        *
060000860423     C                   WRITE     OFN220T1                                     *
060100860423     C*---------- EMISSIONE FONDO SUBFILE -2- -------------*
060200860423     C     TAG100        TAG                                                    *
060300860505     C                   SETOFF                                       11  17    *
060400860424     C                   Z-ADD     0             SC2REC                         *
060500860421     C                   Z-ADD     0             SC2VID                         *
060600860424     C                   Z-ADD     0             SC2CMB                         *
060700990614     C                   move      *blank        SC2CDC
060800860424     C                   MOVE      *ZEROS        SC2ART                         *
060900860424     C                   MOVE      *BLANKS       SC2DMT                         *
061000860424     C                   MOVE      *BLANKS       SC2UMQ                         *
061100860424     C                   MOVE      *ZEROS        SC2QTA                         *
061200860424     C                   MOVE      *ZEROS        SC2PRZ                         *
061300860424     C                   MOVE      *BLANKS       SC2VAL                         *
061400860421     C                   MOVE      *BLANKS       SC2TRG                         *
061500860421     C                   MOVE      *BLANKS       SC2DON                         *
061600860421     C                   MOVE      *BLANKS       SC2COM                         *
061700860421     C                   Z-ADD     0             SC2IMO                         *
061800860424     C  N11
061900860424     CANN41              SETON                                            17    * RIPART.GLOBAL
062000860423     C*---------- EMISSIONE SUBFILE -2- -------------------*
062100000000     C     TAG102        TAG                                                    *
062200000000     C                   EXSR      EMISF2                                       *
062300000000     C*--- LEGGO RECORD MODIFICATI SU SFL2 ----------------*
062400000000     C                   Z-ADD     0             TOTQTA           11 2          *
062500000000     C                   SETOFF                                       080942    *
062600860424     C                   SETOFF                                       11        *
062700000000     C     1             DO        RECT2         REC2                           *
062800000000     C     REC2          CHAIN     SFL2                               99        * AIUTO !
062900000000     C   99              GOTO      TAG010                                       *
063000860424     C     SF2SCE        CABEQ     ' '           TAG130                         *
063100860421     C*--- RICHIESTA VARIAZIONE PREZZO --------------------*
063200860424     C  N08
063300860424     CANN09SF2SCE        COMP      'V'                                    08    * VARIAZ.PREZZO
063400860423     C*---------- VARIAZIONE PREZZO NON AMMESSA------------*
063500860423     C   08
063600860423     CAN 12              DO                                                     *
063700860505     C                   SETON                                            80    *
063800860505     C   80              MOVEL     ERR(16)       ERRORE                         *
063900860505     C   80              GOTO      TAG100                                       *
064000860423     C                   END                                                    *
064100860423     C*---------- INCONTRO SCELTA -X- :                    *
064200860424     C   08SF2SCE        IFEQ      'X'                                          *
064300860424     C                   MOVE      *BLANKS       SF2SCE                         *
064400000000     C                   UPDATE    SFL2                                         *
064500000000     C                   GOTO      TAG130                                       *
064600860421     C*----------                                          *
064700860423     C                   ELSE                                                   *
064800000000     C                   SETON                                        75        * BL+ RI
064900000000     C                   UPDATE    SFL2                                         *
065000000000     C                   SETOFF                                       75        *
065100860421     C                   Z-ADD     REC2          CURSOR               11        * POSIZ.CURSORE
065200860424     C                   Z-ADD     SF2REC        SC2REC                         * NRR FIS.FONDO
065300860421     C                   Z-ADD     SF2VID        SC2VID                         * NRR VID.FONDO
065400860424     C                   MOVE      SF2ART        SC2ART                         *
065500990614     C                   movel     SF2CDC        SC2CDC
065600860424     C                   MOVEL     SF2DMT        SC2DMT                         *
065700860424     C                   MOVE      SF2UMQ        SC2UMQ                         *
065800860424     C                   Z-ADD     SF2QTA        SC2QTA                         *
065900860423     C                   Z-ADD     SF2PRZ        SC2PRZ                         *
066000860429     C     SF2PRZ        MULT      SF2QTA        PIPPO                          *
066100860429     C     SF2TRG        IFEQ      'D'                                          *
066200860429     C                   SETON                                            24    *
066300860429     C     SC2TOT        ADD       PIPPO         SC2TOT                         *
066400860429     C                   ELSE                                                   *
066500860429     C                   SETOFF                                           24    *
066600860429     C     SC2TOT        SUB       PIPPO         SC2TOT                         *
066700860429     C                   END                                                    *
066800860424     C                   MOVE      SF2VAL        SC2VAL                         *
066900860424     C                   Z-ADD     SF2CMB        SC2CMB                         *
067000860423     C                   MOVE      *BLANKS       ERRORE                         *
067100000000     C*--- EMISSIONE FONDO SFL2 ---------------------------*
067200000000     C     TAG110        TAG                                                    *
067300000000     C                   EXSR      EMISF2                                       *
067400860429     C*--- CONTROLLO PREZZO -------------------------------*
067500860505     C     SC2PRZ        COMP      0                                    8080    *
067600860505     C   80              SETON                                        81        *
067700860505     C   80              MOVEL     ERR(17)       ERRORE                         *
067800860505     C   80              GOTO      TAG110                                       *
067900860429     C*---------- AGGIORNO VARIAZIONE PREZZO --------------*
068000860424     C     SC2VID        CHAIN     SFL2                               99        * AIUTO !
068100000000     C   99              GOTO      TAG010                                       *
068200860424     C                   MOVE      *BLANKS       SF2SCE                         *
068300860429     C     SC2PRZ        MULT      SC2QTA        PIPPO                          *
068400860429     C  N24SC2TOT        ADD       PIPPO         SC2TOT                         *
068500860429     C   24SC2TOT        SUB       PIPPO         SC2TOT                         *
068600860421     C                   MOVE      SC2PRZ        SF2PRZ                         *
068700860424     C                   Z-ADD     SC2CMB        SF2CMB                         *
068800990614     C                   movel     SC2CDC        SF2CDC
068900000000     C                   UPDATE    SFL2                                         *
069000860423     C                   END                                                    *
069100860421     C*--- TOTALIZZAZIONE QUANTITA RIGHE SCELTE X ONERI ---*
069200860424     C  N08
069300860424     CANN09SF2SCE        COMP      'X'                                    09    * RIPART.ONERI
069400860423     C*---------- INCONTRO SCELTA -V- :                    *
069500860424     C   09SF2SCE        IFEQ      'V'                                          *
069600860424     C                   MOVE      *BLANKS       SF2SCE                         *
069700000000     C                   UPDATE    SFL2                                         *
069800000000     C                   GOTO      TAG130                                       *
069900000000     C                   ELSE                                                   *
070000860429     C     SF2TRG        CABEQ     'D'           TAG130                         *
070100860429     C     SF2TRG        CABEQ     'I'           TAG130                         *
070200860429     C     SF2TRG        CABEQ     'S'           TAG130                         *
070300860423     C*---------- CONTROLLO ONERI :                        *
070400860424     C  N42              MOVE      SF2UMQ        SAVUMQ                         *
070500860424     C  N42              EXSR      CTRONE                                       *CTRL. FONDO
070600860423     C  N42              SETON                                        42        *
070700860421     C*--- CONTROLLO UNIVOCITA' U.MIS.:                    *
070800860424     C     SF2UMQ        IFEQ      SAVUMQ                                       *
070900860424     C     TOTQTA        ADD       SF2QTA        TOTQTA                         *
071000860421     C                   ELSE                                                   *
071100860505     C                   SETON                                            80    *
071200860505     C                   MOVEL     ERR(18)       ERRORE                         *
071300000000     C                   GOTO      TAG102                                       *
071400860421     C                   END                                                    *
071500860421     C                   END                                                    *
071600860421     C*----------                                          *
071700000000     C     TAG130        TAG                                                    *
071800000000     C                   END                                                    *
071900860421     C*--- FINE LETTURA RIGHE VARIAZIONE PREZZI -----------*
072000860423     C   08              GOTO      TAG100                                       *
072100860421     C*--- RIPARTIZIONE ONERI SU RIGHE SCELTE--------------*
072200000000     C   09              DO                                                     *
072300000000     C     SC2IMO        DIV       TOTQTA        IMOQTA           11 0          *
072400860423     C                   MVR                     RESTIM           15 243        *
072500860421     C*---------                                           *
072600860424     C     1             DO        RECT2         REC2                           *
072700860424     C     REC2          CHAIN     SFL2                               99        * AIUTO !
072800000000     C   99              GOTO      TAG010                                       *
072900860424     C     SF2SCE        IFEQ      'X'                                          *
073000860429     C     SF2TRG        CABEQ     'D'           NORIPX                         *
073100860429     C     SF2TRG        CABEQ     'I'           NORIPX                         *
073200860429     C     SF2TRG        CABEQ     'S'           NORIPX                         *
073300860423     C*--------- METTO RESTO IN PRIMO RECORD :             *
073400860424     C   43RESTIM        DIV(H)    SF2QTA        RESTIM                         *
073500860424     C   43IMOQTA        ADD       RESTIM        VALONE                         *
073600860423     C*--------- RIPARTIZIONE ONERE SU MOVIMENTI :         *
073700860424     C  N43              Z-ADD     IMOQTA        VALONE                         *
073800000000     C                   SETOFF                                       43        *
073900000000     C     SC2TRG        IFEQ      'D'                                          *
074000860424     C     SF2ONE        SUB       VALONE        SF2ONE                         *
074100000000     C                   ELSE                                                   *
074200860424     C     SF2ONE        ADD       VALONE        SF2ONE                         *
074300000000     C                   END                                                    *
074400860505     C                   MOVE      'R'           SF2RIP                         *
074500860429     C*---------                                           *
074600860429     C     NORIPX        TAG                                                    *
074700860424     C                   MOVE      *BLANKS       SF2SCE                         *
074800000000     C                   UPDATE    SFL2                                         *
074900860429     C                   END                                                    *
075000860423     C*---------                                           *
075100000000     C                   END                                                    *
075200860423     C*--------- SCRIVO RIGA ONERE SU SUBFILE -------------*
075300860505     C                   MOVE      *BLANKS       SF2RIP                         *
075400000000     C                   EXSR      AGGONE                                       *
075500000000     C                   END                                                    *
075600860421     C*--- RIGA DA NON RIPARTIRE --------------------------*
075700860421     C  N08
075800860421     CANN09              DO                                                     *
075900860424     C                   EXSR      CTRONE                                       *
076000860505     C                   MOVE      'O'           SF2RIP                         * ONERE
076100860421     C                   EXSR      AGGONE                                       *
076200860421     C                   END                                                    *
076300000000     C*--- FINE LAVORO ------------------------------------*
076400000000     C     FINE          TAG                                                    *
076500000000     C                   SETON                                            LR    *
076600890704     C*----------------------------------------------------*
076700890704     C     RICNPR        BEGSR
076800890704     C**  RICERCA NUMERO PROTOCOLLO IVA
076900890704     C     SUDT3         TAG                                                    *
077000890704     C     *LOCK         IN        OFNNPR                               47      *
077100890704     C   47              GOTO      SUDT3                                        *
077200890704     C                   OUT       OFNNPR                                       *
077300890704     C                   ADD       1             NPRNRG
077400890704     C                   ENDSR
077500890704     C*----------------------------------------------------*
077600890704     C     AGGNPR        BEGSR
077700890704     C** AGGIORNA DATA AREA OFNNPR
077800890704     C     SUDT2         TAG                                                    *
077900890704     C     *LOCK         IN        OFNNPR                               48      *
078000890704     C   48              GOTO      SUDT2                                        *
078100890704     C                   Z-ADD     SC1NRG        NPRNRG
078200890704     C                   Z-ADD     AMGSC1        NPRDRG
078300890704     C                   OUT       OFNNPR
078400890704     C                   ENDSR
078500890704     C*----------------------------------------------------*
078600860424     C**--- CONTROLLO IMPUTAZIONE ONERI ------------------**
078700860424     C***------------------------------------------------***
078800860424     CSR   CTRONE        BEGSR                                                  *
078900860424     C     BEGON2        TAG                                                    *
079000860424     C*--- CONTROLLO TIPO RIGHE ONERI ---------------------*
079100860424     C     SC2TRG        CABEQ     *BLANKS       TAG100                         *
079200860424     C     SC2TRG        COMP      'C'                                    46    *
079300860424     C  N46SC2TRG        COMP      'D'                                    46    *
079400860424     C  N46SC2TRG        COMP      'I'                                    46    *
079500860424     C  N46SC2TRG        COMP      'S'                                    46    *
079600860424     C  N46              DO                                                     *
079700860505     C                   SETON                                        82  80    *
079800860505     C                   MOVEL     ERR(19)       ERRORE                         *
079900860424     C                   EXSR      EMISF2                                       *
080000860424     C                   GOTO      BEGON2                                       * CONTR.RIGHE X
080100860424     C                   END                                                    *
080200860424     C*--- CONTROLLO DESCRIZIONE ONERI---------------------*
080300860424     C     SC2DON        IFEQ      *BLANKS                                      *
080400860505     C                   SETON                                        83  80    *
080500860505     C                   MOVEL     ERR(20)       ERRORE                         *
080600860424     C                   EXSR      EMISF2                                       *
080700860424     C                   GOTO      BEGON2                                       * CONTR.RIGHE X
080800860424     C                   END                                                    *
080900860424     C*--- CONTROLLO IMPORTO ONERI ------------------------*
081000860424     C     SC2IMO        IFLE      *ZEROS                                       *
081100860505     C                   SETON                                        84  80    *
081200860505     C                   MOVEL     ERR(21)       ERRORE                         *
081300860424     C                   EXSR      EMISF2                                       *
081400860424     C                   GOTO      BEGON2                                       * CONTR.RIGHE X
081500860424     C                   END                                                    *
081600860424     CSR                 ENDSR                                                  *
081700000000     C*--- ROUTINE EMISSIONE SUBFILE "2" ------------------*
081800000000     C***------------------------------------------------***
081900000000     CSR   EMISF2        BEGSR                                                  *
082000000000     C     CURSOR        DIV       10            CURSOR                         * POSIZ.CURSORE
082100860423     C                   MVR                     RESTO             5 046        *
082200000000     C     CURSOR        MULT      10            CURSOR                         *
082300860423     C   46CURSOR        ADD       1             CURSOR                         *
082400000000     C                   EXFMT     SFL2CT                                       * SFL1CT
082500000000     C                   MOVEA     *ZEROS        *IN(75)                        *
082600860424     C                   MOVE      *BLANKS       ERRORE                         *
082700000000     C*--- FINE LAVORO ------------------------------------*
082800990616     c   kc              goto      tag009
082900860421     C*--------- RIPARTIZIONE ONERI -----------------------*
083000860421     C   KG              EXSR      RIPART                                       *
083100990616     C*--------- AGGIORNAMENTO CONTABILITA ----------------*
083200990616     C   KE              EXSR      AGGCNT                                       *
083300000000     CSR                 ENDSR                                                  *
083400850709     C***------------------------------------------------***
083500850709     C**--- EMISSIONE STAMPA TESTATA/DETTAGLIO -----------**
083600850709     C***------------------------------------------------***
083700850709     CSR   RUTSTA        BEGSR                                                  *
083800990614     C                   move      MOFDOR        DATAIso                        *
083900990614     C                   move      dataiso       dataeur                        *
084000990614     C     *dmy          move      dataeur       STADOR            6 0          *
084100850722     C     PAGE1         IFGE      50                                           * TESTATA
084200850709     C                   EXCEPT    TESTA                                        *
084300850709     C                   Z-ADD     0             PAGE1                          *
084400850709     C                   END                                                    *
084500850722     C                   EXCEPT    DETAIL                                       * DETTAGLIO
084600850709     CSR                 ENDSR                                                  *
084700000000     C***------------------------------------------------***
084800000000     C**--- ROUTINE CARICA RIGA ONERI SU SUBFILE 2 -------**
084900000000     C***------------------------------------------------***
085000000000     CSR   AGGONE        BEGSR                                                  *
085100860424     C     RECT2         ADD       1             RECT2                71        *
085200860424     C                   Z-ADD     RECT2         CURSOR                         * POSIZ.CURSORE
085300860424     C                   Z-ADD     RECT2         SF2VID                         * NRR VID.FONDO
085400860424     C                   Z-ADD     0             SF2REC                         * NRR FIS.FONDO
085500860424     C                   MOVE      SC2TRG        SF2TRG                         *
085600860424     C                   MOVEL     SC1CPG        SF2CPG                         *
085700860424     C                   MOVE      SC1VAL        SF2VAL                         *
085800860424     C                   Z-ADD     SC1CMB        SF2CMB                         *
085900990614     C                   move      *blank        SF2CDC
086000860424     C                   MOVEL     SC2DON        SF2DMT                         *
086100860424     C                   MOVEL     SC2COM        SF2COM                         *
086200860505     C                   Z-ADD     0             SF2AIA                         *
086300860528     C                   Z-ADD     1             SF2FCQ                         *
086400860424     C                   MOVE      *BLANKS       SF2SCE                         *
086500860424     C                   MOVE      *BLANKS       SF2ART                         *
086600860424     C                   MOVEL     SC2DON        SF2DES                         *
086700860424     C                   MOVE      'NR '         SF2UMQ                         *
086800860424     C                   Z-ADD     1             SF2QTA                         *
086900860424     C                   Z-ADD     SC2IMO        SF2PRZ                         *
087000860423     C                   Z-ADD     0             SF2ONE                         *
087100860429     C*---------- AGGIORNO TOTALE -------------------------*
087200860429     C     SC2TRG        IFEQ      'D'                                          *
087300860429     C     SC2TOT        SUB       SC2IMO        SC2TOT                         *
087400860429     C                   ELSE                                                   *
087500860429     C     SC2TOT        ADD       SC2IMO        SC2TOT                         *
087600860429     C                   END                                                    *
087700860429     C*----------  SCRIVO SUBFILE -------------------------*
087800000000     C                   SETON                                            74    *
087900000000     C                   WRITE     SFL2                                         *
088000000000     C                   SETOFF                                           74    *
088100860421     C                   GOTO      TAG100                                       *
088200000000     CSR                 ENDSR                                                  *
088300860421     C***------------------------------------------------***
088400860421     C**--- RIPARTIZIONE ONERI SU TUTTE LE RIGHE ---------**
088500860421     C***------------------------------------------------***
088600860421     CSR   RIPART        BEGSR                                                  *
088700860423     C                   Z-ADD     0             TOTQTA               42        *
088800860424     C                   EXSR      CTRONE                                       *
088900860429     C                   SETOFF                                           17    *
089000860421     C     1             DO        RECT2         REC2                           *
089100860421     C     REC2          CHAIN     SFL2                               99        * AIUTO !
089200860421     C   99              GOTO      TAG010                                       *
089300860423     C*------------ TOTALIZZO QUANTITA' -------------------*
089400860429     C     SF2TRG        CABEQ     'D'           NOTOT                          *
089500860429     C     SF2TRG        CABEQ     'I'           NOTOT                          *
089600860429     C     SF2TRG        CABEQ     'S'           NOTOT                          *
089700860423     C*---------- CONTROLLO UNIVOCITA' U.MIS.:             *
089800860424     C  N42              MOVE      SF2UMQ        SAVUMQ                         *
089900860424     C  N42              SETON                                            42    *
090000860424     C     SF2UMQ        IFEQ      SAVUMQ                                       *
090100860424     C     TOTQTA        ADD       SF2QTA        TOTQTA                         *
090200860423     C                   ELSE                                                   *
090300860423     C                   SETON                                            90    *
090400860505     C                   MOVEL     ERR(18)       ERRORE                         *
090500860423     C                   GOTO      TAG102                                       *
090600860423     C                   END                                                    *
090700860429     C     NOTOT         TAG                                                    *
090800860421     C                   END                                                    *
090900860421     C*---------- RIPARTIZIONE QUANTITA' :                 *
091000860423     C     SC2IMO        DIV       TOTQTA        IMOQTA                         *
091100860423     C                   MVR                     RESTIM               43        *
091200860424     C     1             DO        RECT2         REC2                           *
091300860424     C     REC2          CHAIN     SFL2                               99        * AIUTO !
091400860421     C   99              GOTO      TAG010                                       *
091500860424     C     SF2TRG        CABEQ     'D'           NORIPA                         *
091600860424     C     SF2TRG        CABEQ     'I'           NORIPA                         *
091700860424     C     SF2TRG        CABEQ     'S'           NORIPA                         *
091800860424     C   43RESTIM        DIV(H)    SF2QTA        RESTIM                         *
091900860424     C   43IMOQTA        ADD       RESTIM        VALONE                         *
092000860424     C  N43              Z-ADD     IMOQTA        VALONE                         *
092100860421     C                   SETOFF                                       43        *
092200860421     C     SC2TRG        IFEQ      'D'                                          *
092300860424     C     SF2ONE        SUB       VALONE        SF2ONE                         *
092400860421     C                   ELSE                                                   *
092500860424     C     SF2ONE        ADD       VALONE        SF2ONE                         *
092600860421     C                   END                                                    *
092700860505     C                   MOVE      'R'           SF2RIP                         *
092800860421     C                   UPDATE    SFL2                                         *
092900860423     C     NORIPA        TAG                                                    *
093000860421     C                   END                                                    *
093100860421     C*--- SCRIVO RIGA ONERE SU SUBFILE -------------------*
093200860505     C                   MOVE      *BLANKS       SF2RIP                         *
093300860421     C                   EXSR      AGGONE                                       *
093400860421     CSR                 ENDSR                                                  *
093500860505     C***------------------------------------------------***
093600860505     C*-------- CONTABILIZZA FATTURA ----------------------*
093700860505     C***------------------------------------------------***
093800860505     C     AGGCNT        BEGSR                                                  *
093900860505     C*--- AGGIORNAMENTO ARCHIVI --------------------------*
094000860505     C                   EXSR      AGGPOR                                       *
094100860505     C*--- INCREMENTO NUMERO REGISTRAZIONE ----------------*
094200860505     C     SC1NRG        ADD       1             SC1NRG                         *
094300890928     C  N10              MOVEL     ERR(26)       ERRORE                         *
094400860505     C                   MOVE      SC1NFT        ERRORE                         *
094500860505     C                   SETON                                            80    *
094600860530     C                   GOTO      TAG009                                       *
094700860505     CSR                 ENDSR                                                  *
094800990618     C*-------- ------------------------- -----------------*
094900860424     C*-------- AGGIORNAMENTO PORTAFOGLIO -------------1---*
095000990618     C*-------- ------------------------- -----------------*
095100860424     CSR   AGGPOR        BEGSR                                                  *
095200860429     C                   OPEN      OFPRTSTD                             98      *
095300860429     C                   Z-ADD     51            PAGE1             3 0          *
095400860429     C                   Z-ADD     0             TOTMER           11 2          *
095500860424     C     1             DO        RECT2         REC2                           *
095600860429     C                   SETON                                            20    *
095700860429     C                   Z-ADD     0             STAPZ1           11 2          *
095800860429     C                   Z-ADD     0             DELTA             5 2          *
095900860429     C                   SETOFF                                       212223    *
096000860424     C     REC2          CHAIN     SFL2                               99        * AIUTO !
096100860424     C   99              GOTO      TAG010                                       *
096200860429     C*---------                                           *
096300860424     C     SF2REC        IFNE      0                                            *
096400860424     C     SF2REC        CHAIN     ORDFIS                             99        * AIUTO !
096500860424     C   99              GOTO      TAG010                                       *
096600860429     C                   SETOFF                                           20    *
096700860424     C*----------- CALCOLO PREZZO SCONTATO                 *
096800860424     C     MOFPRZ        MULT      MOFSC1        SCONTO                         *
096900860424     C     SCONTO        DIV       100           SCONTO                         *
097000860424     C     MOFPRZ        SUB       SCONTO        MOFPRZ                         *
097100860424     C     MOFPRZ        MULT      MOFSC2        SCONTO                         *
097200860424     C     SCONTO        DIV       100           SCONTO                         *
097300860424     C     MOFPRZ        SUB       SCONTO        MOFPRZ                         *
097400870202     C     MOFPRZ        MULT      MOFSC3        SCONTO                         *
097500870202     C     SCONTO        DIV       100           SCONTO                         *
097600870202     C     MOFPRZ        SUB       SCONTO        MOFPRZ                         *
097700860429     C                   Z-ADD     MOFPRZ        STAPZ1                         *
097800860429     C*----------- ESAMINO SCOSTAMENTO PREZZO -------------*
097900860429     C     SF2PRZ        IFNE      MOFPRZ                                       * PREZZI DIVERS
098000860429     C     SF2PRZ        SUB       MOFPRZ        PERCO            15 5          *
098100860429     C     PERCO         DIV       MOFPRZ        PERCO                          *
098200860429     C     PERCO         MULT      100           DELTA                          *
098300860429     C                   END                                                    *
098400860424     C*---------- AGGIORNAMENTO MAGAZZINO -----------------*
098500860429     C     MOFCRG        IFNE      0                                            *
098600990614     C     MOFCRG        CHAIN     tmmmp05L                           98        * AIUTO !
098700860429     C  N98              DO                                                     *
098800860429     C     SF2PRZ        MULT      SF2CMB        VIDPRZ           15 2          *
098900860429     C     SF2ONE        MULT      SF2CMB        VIDONE           15 2          *
099000860429     C*-----------                                         *
099100860429     C     MOFFAS        IFEQ      'BB'                                         *
099200860429     C     VIDPRZ        ADD       VIDONE        MMPPRZ               23        *
099300860528     C     MMPPRZ        DIV       SF2FCQ        MMPPRZ                         *
099400990614     C                   UPDATE    tmmmp000                                     *
099500860424     C                   ELSE                                                   *
099600860528     C     VIDONE        DIV       SF2FCQ        VIDONE                         *
099700860429     C     MMPPRZ        ADD       VIDONE        MMPPRZ               23        *
099800990614     C                   UPDATE    tmmmp000                                     *
099900860429     C                   END                                                    *
100000860424     C                   END                                                    *
100100860429     C                   END                                                    *
100200860424     C                   END                                                    *
100300860424     C*-------- AGGIORNAMENTO PORTAFOGLIO -------------2---*
100400860429     C   20
100500860429     CANN12              MOVE      'F'           MOFATT                         *
100600860429     C   20
100700860429     CAN 12              MOVE      'S'           MOFATT                         *
100800990614     C                   move      xsccut        MOFUTE                         *
100900990614     C                   Z-ADD     woggig        MOFDMN                         *
101000860429     C   20              MOVE      CODFOR        MOFFOR                         *
101100990614     C   20              z-add     woggig        MOFDOR                         *
101200990614     C   20              move      1231          MOFDOR                         *
101300860429     C   20              Z-ADD     90000         MOFNOR                         *
101400860429     C   20              MOVE      'ON'          MOFTOR                         *
101500860429     C   20              MOVE      '9'           MOFPTY                         *
101600860429     C*----------- ESEGUO STAMPA --------------------------*
101700860429     C     SF2COM        COMP      *BLANKS                            2121      *
101800860429     C                   Z-ADD     0             STAPZ2           11 2          *
101900860429     C  N12                                                                     *
102000860429     COR 20
102100860429     CAN 12              Z-ADD     SF2PRZ        STAPZ2                         *
102200860429     C  N20
102300860429     CAN 12              Z-ADD     SF2ONE        STAPZ2                         *
102400860505     C                   SETOFF                                           25    *
102500860505     C   20SF2TRG        COMP      'C'                                    25    *
102600860505     C   20
102700860505     CANN25SF2TRG        COMP      'D'                                    25    *
102800860505     C   20
102900860505     CANN25SF2TRG        COMP      'I'                                    25    *
103000860505     C   20
103100860505     CANN25SF2TRG        COMP      'S'                                    25    *
103200860505     C   20
103300860505     CAN 25SF2RIP        COMP      *BLANKS                                25    *
103400860506     C     SF2TRG        COMP      'D'                                    27    *
103500860506     C  N27SF2TRG        COMP      'I'                                    27    *
103600860506     C  N27SF2TRG        COMP      'S'                                    27    *
103700860429     C                   EXSR      RUTSTA                                       *
103800860429     C*----------------------------------------------------*
103900990614     C   20              Z-ADD     woggig        MOFDCN                         *
104000860429     C   20              MOVE      '999'         MOFLIS                         *
104100860429     C   20              MOVE      *BLANKS       MOFCCO                         *
104200860429     C  N12                                                                     *
104300860429     COR 20
104400860429     CAN 12              MOVEL     SF2CPG        MOFCPG                         *
104500860429     C  N12                                                                     *
104600860429     COR 20
104700860429     CAN 12              MOVE      SF2VAL        MOFVAL                         *
104800860429     C  N12                                                                     *
104900860429     COR 20
105000860429     CAN 12              Z-ADD     SF2CMB        MOFCMB                         *
105100860429     C   20              MOVE      'S'           MOFFES                         *
105200860505     C*-------- AGGIORNAMENTO PORTAFOGLIO -------------3---*
105300860505     C***------------------------------------------------***
105400860506     C   20              MOVE      SF2TRG        MOFTRG                         *
105500860506     C                   MOVE      'FA'          MOFFAS                         *
105600860506     C   20              MOVE      *BLANKS       MOFTSR                         *
105700860506     C   20              MOVEL     SF2ART        MOFMAT                         *
105800860506     C   20              MOVEL     SF2DMT        MOFDMT                         *
105900860429     C   20              MOVEL     SF2UMQ        MOFUMQ                         *
106000860429     C   20              Z-ADD     SF2QTA        MOFQTA                         *
106100860424     C                   Z-ADD     SF2PRZ        MOFPRZ                         *
106200860424     C                   Z-ADD     0             MOFSC1                         *
106300860424     C                   Z-ADD     0             MOFSC2                         *
106400870202     C                   Z-ADD     0             MOFSC3                         *
106500860429     C   20              MOVEL     SF2COM        MOFCOM                         *
106600860429     C   20              MOVE      '9999999'     MOFIMB                         *
106700860429     C   20              Z-ADD     MOFDOR        MOFDBL                         *
106800860429     C   20              Z-ADD     90000         MOFNBL                         *
106900860429     C  N12                                                                     *F.MERCE
107000860429     COR 20
107100860429     CAN 12              DO                                                     *F.SERVIZI
107200990614     C                   move      SC1DFT        DATAeur                        *
107300990614     C                   move      dataeur       dataiso                        *
107400990614     C                   move      DATAIso       MOFDFT                         *
107500860424     C                   Z-ADD     SC1NFT        MOFNFT                         *
107600860424     C                   Z-ADD     SC1NRG        MOFNPT                         *
107700990614     C                   move      SC1DRG        DATAeur                        *
107800990614     C                   move      dataeur       dataiso                        *
107900990614     C                   move      DATAIso       MOFDPT                         *
108000860424     C                   END                                                    *
108100860429     C   20              MOVE      *BLANKS       MOFMAG                         *
108200890630     C   20              MOVE      SF2CDC        MOFCDC                         *
108300860429     C   20              Z-ADD     0             MOFCRG                         *
108400860429     C   20              MOVE      '999'         MOFDEM                         *
108500860429     C   20              MOVE      '99'          MOFGPM                         *
108600860429     C   20              MOVE      '99'          MOFSTM                         *
108700860429     C   20              MOVE      *BLANKS       MOFFLG                         *
108800170321     c                   exsr      GesStime
108900860429     C  N20              UPDATE    ORDFIS                                       *
109000860429     C   20              WRITE     ORDFIS                                       *
109100860424     C                   END                                                    *
109200860429     C                   EXCEPT    TOTAL                                        *
109300860505     CSR                 ENDSR                                                  *
109400170321     C*----------------------------------------------------*
109500170321     C* GesStime inserimento modifica dati stime
109600170321     C*----------------------------------------------------*
109700170321     C     GesStime      begsr
109701171102      * considero solo righe con entrata merce magazzino superiore al 1 novembre data CUT-OFF
109702171102     c                   if        mofdbl < 20171101
109703171102     c                   leavesr
109704171102     c                   endif
109800170321      /free
109900170321
109901171019        //ricava variabili da passare a programma
109902171019       if mofflg = *blank;
109903171019       clear idriga;
109904171115       leavesr;
109905171019        else ;
109906171019       idriga = %int(mofflg);
109907171019       endif;
111700170410        // Per l'inserimento delle righe speciali senza articolo forziamo il codice
111800170410       if mofmat <> *blank;
111900170502        chain (mofmat) tmamp01l ;
112000170502         if %found(tmamp01l);
112100170502           articolo = 'ACQ'+ ampscm;
112200170502            else;
112300170502           articolo = 'ACQ00000';
112400170502         endif ;
112500170502       else ;
112600170410          if mofdmt = 'CONAI';
112700170410           articolo = 'ACQCONAI';
112800170410            else;
112900170410           articolo = 'ACQ00000';
113000170410          endif ;
113100170410       endif;
114200170321       prezzo = mofprz * mofqta;
114201171019        kpjbu = IMOFDS00 ;
114202171019
114203171019        //chiamata a pgm scrittura stime
114204171019       YDRTFRJOB(kpjba:pgmname);
114205171019
114206171019        imofds00 = kpjbu ;
114207171019
114208171019        //ritorno se tutto ok trovo un numero positivo in esito che indica la riga di registrazione
114209171019        //che va riportata sul file ordine per essere collegata
114210171019
115900170321       if esito > 0;
116100171019        mofflg=%char(idriga);
116200170502         else;
116300170502        esito=YDRere_GetMessaggi(nrMessaggi : idMessaggio : tipoMessaggio
116400170502              : idCampo : testoMessaggio);
116500170502        exsr  InvMail;
116600170321       endif;
116700170321
116800170321
116900170321      /end-free
117000170321     C                   ENDSR
117100170502     C*----------------------------------------------------*
117200170502     C* InvMail  invia mail segnalazione
117300170502     C*----------------------------------------------------*
117400170502     C     InvMail       begsr
117500170502     c                   move      mofdor        dataiso
117600170502     c                   move      dataiso       dataeur
117700170502     c                   move      dataeur       dataord           8 0
117800170502     C* Inizializzo variabili
117900171019      *    SE non aggiunge o aggiorna invia mail segnalazione
118000170502     C                   movel     *blanks       wrkEml          253
118100170502     C                   movel     *blanks       wrkEmcc         253
118200170502     C                   movel     *blanks       wrkMsg         5000
118300170502     C                   movel     *blanks       wrkOgg           44
118400170502     C                   movel     *blanks       wrkesit           1
118401171115     c                   if        %subst(knsif:7:1) = 'P'
118402171115     C                   eval      wrkEml= 'Michele.malipiero@brt.it'
118403171115     c                   else
118500170503     C                   eval      wrkEml= 'Acquisti@BRT.it'
118501171115     c                   endif
118600170502
118701171025     C                   eval      wrkOgg= 'FATTURE DA RICEVERE da -
118800171025     C                              Inserimento Fatture  non riuscito'
118900171025     C                   eval      wrkMsg= 'Riga Ordine Nr. '
119000170502     C                             +  %editc(MofNor:'Z')
119100170502     C                             + '/ ' + %editc(dataord:'Y')
119200170502     c                             + ' Fornitore ' + moffor
119300170502     c                             + ' Articolo '
119400170502     C                             + '/ ' + mofmat + ' / ' + mofdmt
119401171025     C                             + '/ ' + ARTICOLO
119500170502     c                             + ' Quantità '
119600170502     C                             + '/ ' + %editc(mofqta:'2')
119700170502     c                             + ' Prezzo '
119800170502     C                             + '/ ' + %editc(mofprz:'2') +':/N' +
119900170502     C                             %editc(Nrmessaggi:'Z') + ' '
120000170502     C                             + testomessaggio(1) +':/N'
120100170502     C                             + testomessaggio(2) +':/N'
120200170502     C                             + testomessaggio(3) +':/N'
120300170502     C*
120400170502     C                   call(e)   'TIS701C1'
120500170502     C                   parm                    wrkEml
120600170502     C                   parm                    wrkEmcc
120700170502     C                   parm                    wrkOgg
120800170502     C                   parm                    wrkMsg
120900170502     C                   parm                    wrkesit
121000170502     c                   endsr
121100990614     C*----------------------------------------------------*
121200990614     C* Reperimento dati società
121300990614     C*----------------------------------------------------*
121400990614     C     REPSOC        BEGSR
121500990614     C*
121600990614     C                   CALLB     'XSOC'
121700990614     C                   PARM                    TIPXSC            6
121800990614     C                   PARM                    SOCXSC            3
121900990614     C                   PARM                    CDSXSC            9 0
122000990614     C                   PARM                    MODXSC            3
122100990614     C                   PARM      *blanks       RTNXSC            1
122200990614     C                   PARM                    XSOCDS
122300990614     C                   PARM                    KPJBA
122400990614     C*
122500990614     C                   ENDSR
122600850709     O*--------------------------------------------------------------*
122700850709     O*     TESTATA                                                  *
122800850709     O*--------------------------------------------------------------*
122900860429     OOFPRTSTD  E            TESTA            01
123000860421     O                       SK1                132
123100850709     O                       DESDIT              20
123200990618     O                       woggi              120 '  /  /    '
123300860429     O                       PAGE          Z    132
123400860429     O          E            TESTA       1
123500860421     O                       SK2                132
123600860429     O          E            TESTA       1
123700860421     O                       SK3                132
123800850709     O                       KNMUS              132
123900860429     O          E            TESTA       2
124000860429     O                                           53 '*--Registr.--del------'
124100860429     O                                           75 'Pag.-Val.-------Import'
124200860429     O                                           94 'o--------Cambio---*'
124300860429     O          E            TESTA       1
124400990618     O                       SC1NRG              39
124500990614     O                       SC1DRG              50 '  /  /    '
124600860429     O                       SC1CPG              56
124700860429     O                       SC1VAL              62
124800860429     O                       SC1IMP              80 ' .   .   . 0 ,  -'
124900860429     O                       SC1CMB              91 '  . 0 ,  '
125000860429     O          E            TESTA       0  0
125100990618     O                       SC1NRG              39
125200990614     O                       SC1DRG              50 '  /  /    '
125300860429     O                       SC1CPG              56
125400860429     O                       SC1VAL              62
125500860429     O                       SC1IMP              80 ' .   .   . 0 ,  -'
125600860429     O                       SC1CMB              91 '  . 0 ,  '
125700860429     O          E            TESTA       2
125800860421     O                       SK4                132
125900850710     O                       CODFOR              65
126000850710     O                       NOMFOR              96
126100860429     O              N12                         119 'Fattura N.XXXXXXX del'
126200860429     O              N12      SC1NFT             115
126300990618     O              N12      SC1DFT             130 '  /  /    '
126400860429     O          E    12      TESTA       1
126500860421     O                       SK5                132
126600860429     O                       FORSER              65
126700860429     O                       NOMSER              96
126800860429     O                                          119 'Fattura N.XXXXXXX del'
126900860506     O                     26                   119 'Nota Ac.N.XXXXXXX del'
127000860429     O                       SC1NFT             115
127100990618     O                       SC1DFT             130 '  /  /    '
127200860429     O          E            TESTA       2
127300860421     O                       SK6                132
127400860506     O                     26                   123 '-NOTA ACCREDITO-'
127500860429     O          E            TESTA       1
127600860421     O                       SK7                132
127700860429     O                     12                   122 '-----Oneri--'
127800860429     O/EJECT
127900850709     O*--------------------------------------------------------------*
128000850710     O*     DETTAGLIO                                                *
128100850709     O*--------------------------------------------------------------*
128200860429     O          E            DETAIL      1
128300860429     O                       PAGE1              132
128400860429     O                       SK8                132
128500860429     O          E            DETAIL      1
128600860429     O                       PAGE1              132
128700860429     O                       SK8                132
128800860429     O                       MOFNOR        Z      5
128900860429     O                       MOFTOR               8
129000990618     O                       STADOR              17 '  /  /  '
129100871008     O                       SF2ART              30
129200871008     O                  27                       30 '  Onere   '
129300860429     O                       SF2DMT              60
129400860429     O                       SF2TRG              62
129500860429     O                       SF2UMQ              66
129600860429     O                       SF2QTA              77 '     0 ,  '
129700860429     O                 N20   MOFCPG              82
129800860429     O                 N20   MOFVAL              87
129900860429     O                 N20   STAPZ1             103 '   .   . 0 ,  -'
130000860429     O                       SC1VAL             108
130100860429     O                       STAPZ2             124 '   .   . 0 ,  -'
130200860429     O                       DELTA              132 '   ,  -'
130300860506     O                  25                      132 '..(R)..'
130400860429     O          E    21      DETAIL      1
130500860429     O                       PAGE1              132
130600860429     O                       SK8                132
130700860429     O                       SF2COM              50
130800860429     O          E    22      DETAIL      1
130900860429     O                       PAGE1              132
131000860429     O                       SK8                132
131100860429     O                                           40 'Commessa.:'
131200860429     O          E    23      DETAIL      1
131300860429     O                       PAGE1              132
131400860429     O                       SK8                132
131500860429     O                                           50 'Prezzo in Magazzino.'
131600860429     O                       MOFMAG              57
131700990616     O                       MMPPRZ             103 '   .   . 0 ,     -'
131800860429     O/SPACE
131900860429     O*--------------------------------------------------------------*
132000860429     O*     TOTALE                                                   *
132100860429     O*--------------------------------------------------------------*
132200860429     O          E            TOTAL       1
132300860429     O                       PAGE1              132
132400860429     O                       SK8                132
132500860429     O          E            TOTAL       1
132600860429     O                       SK9                132
132700860429     O                       SC1VAL             108
132800860429     O                       SC2TOT             124 '   .   . 0 ,  -'
132900860429     O          E            TOTAL       0  0
133000860429     O                       SC1VAL             108
133100860429     O                       SC2TOT             124 '   .   . 0 ,  -'
133200860429**   ERR  ---------------------------------------ERRORI-------------------------
133300860505*- Fornitore Materiale ERRATO !!!     -*         81        ERR,1
133400860505*- Fornitore Servizi   ERRATO !!!     -*         82        ERR,2
133500860505*- Digitare Numero BOLLA o 9..9  !!!  -*         83        ERR,3
133600860505*- Codice Prodotto ERRATO     !!!     -*         84        ERR,4
133700860505*- Numero/Data Fattura  ERRATO !!!    -*         85        ERR,5
133800860505*- Cod.   Pagamento ERRATO !!!        -*         86        ERR,6
133900860505*- Digitare Importo Fattura !!!       -*         87        ERR,7
134000860505*- Cod.Valuta o Cambio ERRATO !!!     -*         88        ERR,8
134100860505*- Numero/Data Registrazione ERRATO ! -*         89        ERR,9
134200860505*- Non trovati Ordini Corrispondenti  -*         90        ERR,10
134300860505Fornitore in uso da Utente -> xxxxxxxxxx         91        ERR,11
134400860505CMD05 ---> FORZATURA Valuta ------> xxxx         80        ERR,12
134500860505CMD06 ---> FORZATURA Pagamento---->  xxx         80        ERR,13
134600860505*- Valuta e/o Pagamento non Omogenei !-*         80        ERR,14
134700860505*- Selezionare Righe da Estrarre !!!  -*         80        ERR,15
134800860505*- Variazione Prezzo non Ammessa !!!  -*         80        ERR,16
134900860505*- Attenzione al Prezzo del Prodotto !-*         81        ERR,17
135000860505*- U.Misura non omogenee non Ripartibili         80        ERR,18
135100860505*- Tipo Onere Aggiuntivo ERRATO  !!!  -*         82        ERR,19
135200860505*- Inserire Descrizione dell' Onere   -*         83        ERR,20
135300860505*- Inserire Importo dell' Onere  !!!  -*         84        ERR,21
135400860505*- Contabilizzazione non eseguita !!! -*         80        ERR,22
135500860505*- Eseguita Contabilizz. Fattura xxxxxxx         80        ERR,23
135600860505****- ERRORE FILE. CONTATTARE CED. -****         99        ERR,24
135700860505( 9.9 = Tutti )                                            ERR,25
135800890928*- Eseguito variazione ordini Ft xxxxxxx         80        ERR,26
135900860421**   SK1
136000860421*-DESDIT-----------* ***------------------------------------------SK01
136100860421-------------------------------------*** il xx/xx/xx      Pag.    SK01
136200860421**   SK2
136300860429                     ** Gestione Ordini Fornitori : ANALISI degli SK02
136400990614SCOSTAMENTI Ordini/Fatture FORNITORI  **        Pgm...: TII410R   SK02
136500860421**   SK3
136600860421                     ***------------------------------------------SK03
136700860421-------------------------------------***        Utente: xxxxxxxxxxSK03
136800860421**   SK4
136900860429                                Fornitore del Prodotto..: xxxxxxx.SK04
137000860429xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx                                    SK04
137100860421**   SK5
137200860429                                Fornitore di  Servizi...: xxxxxxx.SK05
137300860421xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx                                    SK05
137400860429**   SK6
137500860429*--Ordine---del--*-Prodotto---Descrizione-------------------*T*Um.SK06
137600860429-Quantità--*----O-R-D-I-N-E----------*----F-A-T-T-U-R-A-----Delta-SK06
137700860429**   SK7
137800860429                                                            *r*---SK07
137900860429-----------*Pag.-Val.--Prezzo Netto--*Val.--Prezzo Netto-----%%%-*SK07
138000860429**   SK8
138100860429                 :                                          : :   SK08
138200860429           :                         :                            SK08
138300860429**   SK9
138400860429*----------------:------------------------------------------:-:---SK09
138500860429-----------:---Totale Operazione.....:                    *------*SK09
