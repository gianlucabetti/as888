000100030401     H*--------------------------------------------------------------------------------------------*
000200081030     H* Polling su OUTQ di "IN" x trattamento automatico spool.
000300030401     H*--------------------------------------------------------------------------------------------*
000400030401     H DECEDIT('0,') DATEDIT(*DMY.)
000500030401     H*--------------------------------------------------------------------------------------------*
000600030402     F*
000700990513     D FIELD           DS           128
000800990513     D  FJOB                  13     22
000900990513     D  FUSR                  23     32
001000990513     D  FJOBN                 33     38
001100990513     D  FSPL                  39     48
001200990513     D  FSPLN                 49     52B 0
001300001006     D*
001400990513     D RCVVAR          DS
001500990513     D  OFFSET                 1      4B 0
001600990513     D  NOENTR                 9     12B 0
001700990513     D  LSTSIZ                13     16B 0
001800990513     D                 DS
001900990513     D  RCVLEN                 9     12B 0
002000990513     D*-----------------------------------------
002100990513     D* DS  PSSPLA0100 DI QUSRTOOL/QATTRPG
002200990513     D*-----------------------------------------
002300990513     D RCVAR1          DS
002400990513     D  BYTRTN                 1      4B 0
002500990513     D  BYTVAL                 5      8B 0
002600990513     D  IJOBID                 9     24
002700990513     D  ISPLID                25     40
002800990513     D  JOBNAM                41     50
002900990513     D  USRNAM                51     60
003000990513     D  JOBNUM                61     66
003100990513     D  FILNAM                67     76
003200990513     D  FILNUM                77     80B 0
003300990513     D  FRMTYP                81     90
003400990513     D  USRDTA                91    100
003500990513     D  STATUS               101    110
003600990513     D  FILAVL               111    120
003700990513     D  HLDF                 121    130
003800990513     D  SAVF                 131    140
003900990513     D  TOTPAG               141    144B 0
004000990513     D  PAGWRT               145    148B 0
004100990513     D  STRPAG               149    152B 0
004200990513     D  ENDPAG               153    156B 0
004300990513     D  LASPAG               157    160B 0
004400990513     D  RESPRT               161    164B 0
004500990513     D  TOTCPY               165    168B 0
004600990513     D  CPYLFT               169    172B 0
004700990513     D  LPI                  173    176B 0
004800990513     D  CPI                  177    180B 0
004900990513     D  OUTPRI               181    182
005000990513     D  OUTQNM               183    192
005100990513     D  OUTQLB               193    202
005200990513     D  DATFOP               203    209
005300990513     D  TIMFOP               210    215
005400990513     D  DEVFNA               216    225
005500990513     D  DEVFLB               226    235
005600990513     D  PGMOPF               236    245
005700990513     D  PGMOPL               246    255
005800990513     D  ACCCOD               256    270
005900990513     D  PRTTXT               271    300
006000990513     D  RCDLEN               301    304B 0
006100990513     D  MAXRCD               305    308B 0
006200990513     D  DEVCLS               309    318
006300990513     D  PRTTYP               319    328
006400990513     D  DOCNAM               329    340
006500990513     D  FLDNAM               341    404
006600990513     D  S36PRC               405    412
006700990513     D  PRTFID               413    422
006800990513     D  RPLUN                423    423
006900990513     D  RPLCHR               424    424
007000990513     D  PAGLEN               425    428B 0
007100990513     D  PAGWID               429    432B 0
007200990513     D  NUMSEP               433    436B 0
007300990513     D  OVRLIN               437    440B 0
007400990513     D  DBCSDA               441    450
007500990513     D  DBCSEC               451    460
007600990513     D  DBCSSO               461    470
007700990513     D  DBCSCR               471    480
007800990513     D  DBCSCI               481    484B 0
007900990513     D  GRAPHI               485    494
008000990513     D  CODPAG               495    504
008100990513     D  FORNAM               505    514
008200990513     D  FORLIB               515    524
008300990513     D  SRCDRW               525    528B 0
008400990513     D  PRTFON               529    538
008500990513     D  S36SPL               539    544
008600990513     D  PAGROT               545    548B 0
008700990513     D  JUSTIF               549    552B 0
008800990513     D  PRTBOT               553    562
008900990513     D  FLDRCD               563    572
009000990513     D  CTLCHR               573    582
009100990513     D  ALGFRM               583    592
009200990513     D  PRTQUA               593    602
009300990513     D  FRMFED               603    612
009400990513     D  VOLUME               613    683
009500990513     D  FLABID               684    700
009600990513     D  EXCTYP               701    710
009700990513     D  CHRCOD               711    720
009800990513     D  TOTRCD               721    724B 0
009900041001     D  USERTEXT            1017   1116
010000041001     D  USEREXTD            1157   1411
010100081101     D  JOBSYSN             1513   1520
010200081101     D*-----------------------------------------
010300081101     D* DS  RIDEFINIZIONE FLAG OPERATIVI DI PROCEDURA (PARAMETRO IN INPUT)
010400081101     D*-----------------------------------------
010500081101     D DS_FLGOPE       DS
010600081101     D  §OPE_BM                       1A
010700081101     D  §OPE_RNMIFS                   1A
010800081030     D*---
010900081030     D* DS per gestione dati di servizio sulla stampa
011000081030     D*---
011100081030     D trbmm1ds      e ds
011200030404     D*---
011300030404     D* DS per chiamata pgm utilità x passaggio da spool a file *FCFC "normalizzato"
011400030404     D*---
011500081030     D trbmfcds      e ds
011600001006     D*---
011700001006     D* Variabili di wrk
011800001006     D*---
011900990513     D jobinf          ds
012000990513     D    job                        10
012100990513     D    user                       10
012200990513     D    jobn                        6
012300071126     D*
012400081101     D wPATH           S            255    inz
012500070607     D*---
012600070607     D* Costanti
012700070607     D*---
012800081101     D* La costante "dtaqlibdft" è valorizzata espressamente in GAITRAOBJ e nn *LIBL xchè
012900070607     D* l'API che ne reperisce da DTAQ relativa in questo modo è + veloce.
013000081030     D dtaqindft       c                   const('EMAILIN   ')
013100081030     D dtaqoutdft      c                   const('EMAILOUT  ')
013200081030     D fildft          c                   const(*blanks)
013300081030     D idWSCSTdft      c                   const('HP4000A4  ')
013400081030     D pathDESTdft     c                   const(*blanks)
013500081030     D fileDESTdft     c                   const(*blanks)
013600081030     D extDESTdft      c                   const(*blanks)
013700081030     D opzdft          c                   const(*blanks)
013800081101     D flgOPEdft       c                   const(*blanks)
013900081101     D dtaqlibdft      c                   const('GAITRAOBJ ')
014000070607     C*
014100070607     C* Ricevimento parametri
014200070607     C     *ENTRY        PLIST
014300081030     C                   PARM                    p_dtaqIN         10
014400081030     C                   PARM                    p_dtaqOUT        10
014500081030     C                   PARM                    p_fil             3
014600081030     C                   PARM                    p_idWSCST        10
014700081030     C                   PARM                    p_pathDEST      255
014800081030     C                   PARM                    p_fileDEST      255
014900081030     C                   PARM                    p_extDEST        15
015000081030     C                   PARM                    p_opz             2
015100081101     C                   PARM                    p_flgOPE         10
015200081030     C*
015300081030     C* Gestisco il ricevimento parametri
015400081030     C                   IF        %parms <> 9
015500081030     C                   MOVEL(P)  dtaqindft     dtaqIN           10
015600081030     C                   MOVEL(P)  dtaqoutdft    dtaqOUT          10
015700081030     C                   MOVEL(P)  fildft        fil               3
015800081101     C                   MOVEL(P)  idWSCSTdft    idWSCST          10
015900081030     C                   MOVEL(P)  pathDESTdft   pathDEST        255
016000081030     C                   MOVEL(P)  fileDESTdft   fileDEST        255
016100081030     C                   MOVEL(P)  extDESTdft    extDEST          15
016200081030     C                   MOVEL(P)  opzdft        opz               2
016300081101     C                   MOVEL(P)  flgOPEdft     flgOPE           10
016400081030     C                   ELSE
016500081030     C                   MOVEL(P)  p_dtaqIN      dtaqIN
016600081030     C                   MOVEL(P)  p_dtaqOUT     dtaqOUT
016700081030     C                   MOVEL(P)  p_fil         fil
016800081030     C                   MOVEL(P)  p_idWSCST     idWSCST
016900081030     C                   MOVEL(P)  p_pathDEST    pathDEST
017000081030     C                   MOVEL(P)  p_fileDEST    fileDEST
017100081030     C                   MOVEL(P)  p_extDEST     extDEST
017200081030     C                   MOVEL(P)  p_opz         opz
017300081101     C                   MOVEL(P)  p_flgOPE      flgOPE
017400081030     C                   ENDIF
017500081030     C*
017600081030     C* Verifico correttezza parametri
017700081030     C                   IF        dtaqIN = *blanks
017800081030     C                   MOVEL(P)  dtaqindft     dtaqIN
017900081030     C                   ENDIF
018000081030     C                   IF        dtaqOUT = *blanks
018100081030     C                   MOVEL(P)  dtaqoutdft    dtaqOUT
018200081030     C                   ENDIF
018300081030     C                   IF        idWSCST = *blanks
018400081030     C                   MOVEL(P)  idWSCSTdft    idWSCST
018500081030     C                   ENDIF
018600071031     C*
018700081030     C* Se passata anche la filiale la utilizzo nella costruzione della dupla *DTAQ "IN/OUT"
018800081101     C                   IF        fil <> *blanks
018900081101     C                   EVAL      dtaqIN  = %trim(dtaqIN)  + fil
019000081101     C                   EVAL      dtaqOUT = %trim(dtaqOUT) + fil
019100081030     C                   ENDIF
019200081103     C*
019300081103     C* Verifico correttezza formale percorso destinazione
019400081103     C                   EVAL      wPATH = pathDEST
019500081103     C                   EXSR      chkPATH
019600081103     C                   EVAL      pathDEST = wPATH
019700990513     C*
019800040930     C* Sta in ascolto sulla DTAQ "IN" abbinata alla OUTQ "IN"
019900990513     C                   DO        *HIVAL
020000990513     C                   CALL      'QRCVDTAQ'                           21
020100040930     C                   PARM      DTAQIN        QNAME            10
020200081101     C                   PARM      DTAQLIBDFT    QLIB             10
020300990513     C                   PARM      128           FLDLEN            5 0
020400990513     C                   PARM                    FIELD           128
020500990513     C                   PARM      -1            WAIT              5 0
020600990513     C*
020700990513     C                   MOVEL     FJOB          JOB
020800990513     C                   MOVEL     FUSR          USER
020900990513     C                   MOVEL     FJOBN         JOBN
021000990513     C*
021100990513     C* Reperisce attributi del file
021200081030     C                   EVAL      RCVLEN = %size(RCVAR1)
021300990513     C                   CALL      'QUSRSPLA'                           21
021400990513     C                   PARM                    RCVAR1
021500081030     C                   PARM                    RCVLEN
021600990513     C                   PARM      'SPLA0100'    FMTNM1            8
021700990513     C                   PARM                    JOBINF
021800001006     C                   PARM                    JOBid            16
021900990513     C                   PARM                    SPLFID           16
022000990513     C                   PARM                    FSPL
022100990513     C                   PARM                    FSPLN
022200081030     C*
022300081030     C* Elaboro solo gli spool in *OUTQ DI INPUT
022400081030     C                   IF        %scan(dtaqIN:OUTQNM) > *zeros AND
022500081101     C                             %trim(OUTQLB) = dtaqlibdft
022600001006     C*
022700030401     C* Lancia il CL di conversioine spool in file *FCFC nell IFS
022800030401     C                   MOVEL(p)  *ALL'0'       NUMFIL            6
022900030401     C                   MOVE      FILNUM        NUMFIL
023000081030     C                   MOVEL     USEREXTD      TRBMM1DS
023100081030     C*
023200081101     C*******************
023300081030     C* A questo punto applico le opzioni eventualmente ricevute in input
023400081030     C                   IF        opz = '01'
023500081030     C* ... '01' => nome file finale dai dati utenti dello spool
023600081030     C                   EVAL      fileDEST = §CM1VAR
023700081101     C* ... '01' => se presente aggiungo la filiale come sub-folder al percorso destinazione
023800081103     C                   IF        §CM1PO <> *blanks
023900081101     C                   EVAL      wPATH = pathDEST + §CM1PO
024000081101     C                   EXSR      chkPATH
024100081101     C                   EVAL      pathDEST = wPATH
024200081103     C                   ENDIF
024300081030     C                   ENDIF
024400081101     C*******************
024500081101     C*
024600081101     C* Ridefinisco i flag operativi ricevuti in input
024700081101     C                   EVAL      DS_FLGOPE = flgOPE
024800081101     C*******************
024900081103     C*
025000081103     C* Effettuo verifica esistenza (creazione) percorso destinazione
025100081103 ccc C
025200081103     C
025300081103     C
025400081103     C
025500081030     C*
025600081030 ccc C* Effettuo risoluzione costanti figurative eventualmente presenti nel nome file
025700081030     C
025800081030     C
025900081030     C
026000081030     C
026100081030     C*
026200081030     C* Valorizzo la DS pgm utilità TRBMFCC
026300081030     C                   CLEAR                   TRBMFCDS
026400030404     C                   EVAL      DFCFNAM  = FILNAM
026500030404     C                   EVAL      DFCJNAM  = JOBNAM
026600030404     C                   EVAL      DFCUNAM  = USRNAM
026700030404     C                   EVAL      DFCJNUM  = JOBNUM
026800030404     C                   EVAL      DFCFNUM  = NUMFIL
026900081030     C                   EVAL      DFCFOUT  = 'TRBMUWWF'
027000040907     C                   EVAL      DFCLOUT  = 'QTEMP'
027100030514     C                   EVAL      DFCFOPT  = 'R'
027200081030     C*
027300081030     C                   CALL(e)   'TRBM2DIC'
027400081030     C                   PARM                    FILNAM
027500081030     C                   PARM                    JOBNAM
027600081030     C                   PARM                    USRNAM
027700081030     C                   PARM                    JOBNUM
027800081030     C                   PARM                    NUMFIL
027900081030     C                   PARM      *blanks       MESSAGGIO        80
028000081030     C                   PARM      '0'           ESITO             1
028100081030     C                   PARM                    TRBMFCDS
028200081030     C                   PARM                    dtaqIN
028300081030     C                   PARM                    dtaqOUT
028400081030     C                   PARM                    PRTTYP
028500081030     C                   PARM                    §CM1PCL
028600081030     C                   PARM                    idWSCST
028700081030     C                   PARM                    pathDEST
028800081030     C                   PARM                    fileDEST
028900081030     C                   PARM                    extDEST
029000081030     C                   PARM                    opz
029100081101     C                   PARM                    flgOPE
029200081030     C*
029300081030     C* Se richiesto innesco iter Bart Mailing => procedo
029400081101     C                   IF        §OPE_BM = 'S'
029500081030     C*
029600081030     C* Se errore nel CL d invio file nell'IFS => imposto a stato HOLD il record del log del mailing
029700081030     C                   IF        ESITO = '2'
029800081030     C                   EVAL      §CM1STS = 'H'
029900081030     C                   ENDIF
030000081030     C*
030100081030     C* Inserisco il record nel file di log Mailing Automatico
030200081030     C                   MOVE(P)   TOTPAG        WTPAG             9 0
030300081030     C                   CALL      'TRBMM1R1'
030400081030     C                   PARM                    FILNAM
030500081030     C                   PARM                    JOBNAM
030600081030     C                   PARM                    USRNAM
030700081030     C                   PARM                    JOBNUM
030800081030     C                   PARM                    NUMFIL
030900081030     C                   PARM                    JOBSYSN
031000081030     C                   PARM                    USRDTA
031100081030     C                   PARM                    WTPAG
031200081030     C                   PARM      PAGWID        PRTFLEN          10 0
031300081030     C                   PARM      CPI           SPLCPI           10 0
031400081030     C                   PARM                    TRBMM1DS
031500081030     C                   PARM                    PRTTYP
031600081030     C                   PARM                    MESSAGGIO
031700081030     C                   PARM                    ESITO
031800081101     C*
031900081101     C                   ENDIF
032000081030     C*
032100081030     C                   ENDIF
032200001006     C*
032300030401     C                   ENDDO
032400001006     C*
032500030401     C                   RETURN
032600081101
032700081101
032800081101     C*-----------------
032900081101     C* Verifica presenza "/" in fondo al percorso
033000081101     C*-----------------
033100081101     C     chkPATH       BEGSR
033200081101     C*
033300081101     C* All' inzio
033400081101     C                   IF        %subst(%trim(wPATH):1:1)<>'/'
033500081101     C                   EVAL      wPATH = '/' + %trim(wPATH)
033600081101     C                   ENDIF
033700081101     C*
033800081101     C* Alla fine
033900081101     C                   IF        %subst(wPATH:%len(%trim(wPATH)):1)<>'/'
034000081101     C                   EVAL      wPATH = %trim(wPATH) + '/'
034100081101     C                   ENDIF
034200081101     C*
034300081101     C                   ENDSR
034400081101     C*
