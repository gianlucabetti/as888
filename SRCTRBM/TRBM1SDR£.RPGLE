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
011200001006     D*---
011300001006     D* Variabili di wrk
011400001006     D*---
011500990513     D jobinf          ds
011600990513     D    job                        10
011700990513     D    user                       10
011800990513     D    jobn                        6
011900071126     D*
012000081101     D wPATH           S            255    inz
012100070607     D*---
012200070607     D* Costanti
012300070607     D*---
012400081101     D* La costante "dtaqlibdft" è valorizzata espressamente in GAITRAOBJ e nn *LIBL xchè
012500070607     D* l'API che ne reperisce da DTAQ relativa in questo modo è + veloce.
012600081030     D dtaqindft       c                   const('EMAILIN   ')
012700081030     D dtaqoutdft      c                   const('EMAILOUT  ')
012800081030     D fildft          c                   const(*blanks)
012900081030     D idWSCSTdft      c                   const('HP4000A4  ')
013000081030     D pathDESTdft     c                   const(*blanks)
013100081030     D fileDESTdft     c                   const(*blanks)
013200081030     D extDESTdft      c                   const(*blanks)
013300081030     D opzdft          c                   const(*blanks)
013400081101     D flgOPEdft       c                   const(*blanks)
013500081101     D dtaqlibdft      c                   const('GAITRAOBJ ')
013600070607     C*
013700070607     C* Ricevimento parametri
013800070607     C     *ENTRY        PLIST
013900081030     C                   PARM                    p_dtaqIN         10
014000081030     C                   PARM                    p_dtaqOUT        10
014100081030     C                   PARM                    p_fil             3
014200081030     C                   PARM                    p_idWSCST        10
014300081030     C                   PARM                    p_pathDEST      255
014400081030     C                   PARM                    p_fileDEST      255
014500081030     C                   PARM                    p_extDEST        15
014600081030     C                   PARM                    p_opz             2
014700081101     C                   PARM                    p_flgOPE         10
014800081030     C*
014900081030     C* Gestisco il ricevimento parametri
015000081030     C                   IF        %parms <> 9
015100081030     C                   MOVEL(P)  dtaqindft     dtaqIN           10
015200081030     C                   MOVEL(P)  dtaqoutdft    dtaqOUT          10
015300081030     C                   MOVEL(P)  fildft        fil               3
015400081101     C                   MOVEL(P)  idWSCSTdft    idWSCST          10
015500081030     C                   MOVEL(P)  pathDESTdft   pathDEST        255
015600081030     C                   MOVEL(P)  fileDESTdft   fileDEST        255
015700081030     C                   MOVEL(P)  extDESTdft    extDEST          15
015800081030     C                   MOVEL(P)  opzdft        opz               2
015900081101     C                   MOVEL(P)  flgOPEdft     flgOPE           10
016000081030     C                   ELSE
016100081030     C                   MOVEL(P)  p_dtaqIN      dtaqIN
016200081030     C                   MOVEL(P)  p_dtaqOUT     dtaqOUT
016300081030     C                   MOVEL(P)  p_fil         fil
016400081030     C                   MOVEL(P)  p_idWSCST     idWSCST
016500081030     C                   MOVEL(P)  p_pathDEST    pathDEST
016600081030     C                   MOVEL(P)  p_fileDEST    fileDEST
016700081030     C                   MOVEL(P)  p_extDEST     extDEST
016800081030     C                   MOVEL(P)  p_opz         opz
016900081101     C                   MOVEL(P)  p_flgOPE      flgOPE
017000081030     C                   ENDIF
017100081030     C*
017200081030     C* Verifico correttezza parametri
017300081030     C                   IF        dtaqIN = *blanks
017400081030     C                   MOVEL(P)  dtaqindft     dtaqIN
017500081030     C                   ENDIF
017600081030     C                   IF        dtaqOUT = *blanks
017700081030     C                   MOVEL(P)  dtaqoutdft    dtaqOUT
017800081030     C                   ENDIF
017900081030     C                   IF        idWSCST = *blanks
018000081030     C                   MOVEL(P)  idWSCSTdft    idWSCST
018100081030     C                   ENDIF
018200071031     C*
018300081030     C* Se passata anche la filiale la utilizzo nella costruzione della dupla *DTAQ "IN/OUT"
018400081101     C                   IF        fil <> *blanks
018500081101     C                   EVAL      dtaqIN  = %trim(dtaqIN)  + fil
018600081101     C                   EVAL      dtaqOUT = %trim(dtaqOUT) + fil
018700081030     C                   ENDIF
018800081103     C*
018900081103     C* Verifico correttezza formale percorso destinazione
019000081103     C                   EVAL      wPATH = pathDEST
019100081103     C                   EXSR      chkPATH
019200081103     C                   EVAL      pathDEST = wPATH
019300990513     C*
019400040930     C* Sta in ascolto sulla DTAQ "IN" abbinata alla OUTQ "IN"
019500990513     C                   DO        *HIVAL
019600990513     C                   CALL      'QRCVDTAQ'                           21
019700040930     C                   PARM      DTAQIN        QNAME            10
019800081101     C                   PARM      DTAQLIBDFT    QLIB             10
019900990513     C                   PARM      128           FLDLEN            5 0
020000990513     C                   PARM                    FIELD           128
020100990513     C                   PARM      -1            WAIT              5 0
020200990513     C*
020300990513     C                   MOVEL     FJOB          JOB
020400990513     C                   MOVEL     FUSR          USER
020500990513     C                   MOVEL     FJOBN         JOBN
020600990513     C*
020700990513     C* Reperisce attributi del file
020800081030     C                   EVAL      RCVLEN = %size(RCVAR1)
020900990513     C                   CALL      'QUSRSPLA'                           21
021000990513     C                   PARM                    RCVAR1
021100081030     C                   PARM                    RCVLEN
021200990513     C                   PARM      'SPLA0100'    FMTNM1            8
021300990513     C                   PARM                    JOBINF
021400001006     C                   PARM                    JOBid            16
021500990513     C                   PARM                    SPLFID           16
021600990513     C                   PARM                    FSPL
021700990513     C                   PARM                    FSPLN
021800081030     C*
021900081030     C* Elaboro solo gli spool in *OUTQ DI INPUT
022000081030     C                   IF        %scan(dtaqIN:OUTQNM) > *zeros AND
022100081101     C                             %trim(OUTQLB) = dtaqlibdft
022200001006     C*
022300030401     C* Lancia il CL di conversioine spool in file *FCFC nell IFS
022400030401     C                   MOVEL(p)  *ALL'0'       NUMFIL            6
022500030401     C                   MOVE      FILNUM        NUMFIL
022600081030     C                   MOVEL     USEREXTD      TRBMM1DS
022700081030     C*
022800081101     C*******************
022900081030     C* A questo punto applico le opzioni eventualmente ricevute in input
023000081030     C                   IF        opz = '01'
023100081030     C* ... '01' => nome file finale dai dati utenti dello spool
023200081030     C                   EVAL      fileDEST = §CM1VAR
023300081101     C* ... '01' => se presente aggiungo la filiale come sub-folder al percorso destinazione
023400081103     C                   IF        §CM1PO <> *blanks
023500081101     C                   EVAL      wPATH = pathDEST + §CM1PO
023600081101     C                   EXSR      chkPATH
023700081101     C                   EVAL      pathDEST = wPATH
023800081103     C                   ENDIF
023900081030     C                   ENDIF
024000081101     C*******************
024100081101     C*
024200081101     C* Ridefinisco i flag operativi ricevuti in input
024300081101     C                   EVAL      DS_FLGOPE = flgOPE
024400081101     C*******************
024500081103     C*
024600081103     C* Effettuo verifica esistenza (creazione) percorso destinazione
024700081103 ccc C
024800081103     C
024900081030     C*
025000081030 ccc C* Effettuo risoluzione costanti figurative eventualmente presenti nel nome file
025100081030     C
025200081030     C
025300081030     C*
025400160219     C                   CALL(e)   'TRBM2DIC1'
025500081030     C                   PARM                    FILNAM
025600081030     C                   PARM                    JOBNAM
025700081030     C                   PARM                    USRNAM
025800081030     C                   PARM                    JOBNUM
025900081030     C                   PARM                    NUMFIL
026000160219     C                   PARM                    USRDTA
026100081030     C                   PARM      *blanks       MESSAGGIO        80
026200081030     C                   PARM      '0'           ESITO             1
026300081030     C                   PARM                    dtaqIN
026400081030     C                   PARM                    dtaqOUT
026500081030     C                   PARM                    PRTTYP
026600081030     C                   PARM                    §CM1PCL
026700081030     C                   PARM                    idWSCST
026800081030     C                   PARM                    pathDEST
026900081030     C                   PARM                    fileDEST
027000081030     C                   PARM                    extDEST
027100081030     C                   PARM                    opz
027200081101     C                   PARM                    flgOPE
027300081030     C*
027400081030     C* Se richiesto innesco iter Bart Mailing => procedo
027500081101     C                   IF        §OPE_BM = 'S'
027600081030     C*
027700081030     C* Se errore nel CL d invio file nell'IFS => imposto a stato HOLD il record del log del mailing
027800081030     C                   IF        ESITO = '2'
027900081030     C                   EVAL      §CM1STS = 'H'
028000081030     C                   ENDIF
028100081030     C*
028200081030     C* Inserisco il record nel file di log Mailing Automatico
028300081030     C                   MOVE(P)   TOTPAG        WTPAG             9 0
028400081030     C                   CALL      'TRBMM1R1'
028500081030     C                   PARM                    FILNAM
028600081030     C                   PARM                    JOBNAM
028700081030     C                   PARM                    USRNAM
028800081030     C                   PARM                    JOBNUM
028900081030     C                   PARM                    NUMFIL
029000081030     C                   PARM                    JOBSYSN
029100081030     C                   PARM                    USRDTA
029200081030     C                   PARM                    WTPAG
029300081030     C                   PARM      PAGWID        PRTFLEN          10 0
029400081030     C                   PARM      CPI           SPLCPI           10 0
029500081030     C                   PARM                    TRBMM1DS
029600081030     C                   PARM                    PRTTYP
029700081030     C                   PARM                    MESSAGGIO
029800081030     C                   PARM                    ESITO
029900081101     C*
030000081101     C                   ENDIF
030100081030     C*
030200081030     C                   ENDIF
030300001006     C*
030400030401     C                   ENDDO
030500001006     C*
030600030401     C                   RETURN
030700081101
030800081101
030900081101     C*-----------------
031000081101     C* Verifica presenza "/" in fondo al percorso
031100081101     C*-----------------
031200081101     C     chkPATH       BEGSR
031300081101     C*
031400081101     C* All' inzio
031500081101     C                   IF        %subst(%trim(wPATH):1:1)<>'/'
031600081101     C                   EVAL      wPATH = '/' + %trim(wPATH)
031700081101     C                   ENDIF
031800081101     C*
031900081101     C* Alla fine
032000081101     C                   IF        %subst(wPATH:%len(%trim(wPATH)):1)<>'/'
032100081101     C                   EVAL      wPATH = %trim(wPATH) + '/'
032200081101     C                   ENDIF
032300081101     C*
032400081101     C                   ENDSR
032500081101     C*
