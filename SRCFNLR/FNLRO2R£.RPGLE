000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020205     h*
000300020208     Ftitas30c  if   E           K DISK
000400020208     Ftncsa01l  if   E           K DISK
000401121026     Fazorg01l  if   E           K DISK
000600020208     FFICSO00R  UF   E             DISK    USROPN
000700020208     FTNCSB03L  UF   E           K DISK
000800020205     FTNCSV00F  O    E             DISK
000900900604     FCMRIC01L  UF   E           K DISK
001000020212     Ffnlro2p   O    e             PRINTER OFLIND(*IN24)  USROPN
001001121026     Ffnlro2pe  O    e             PRINTER OFLIND(*IN23)  USROPN
001100020212     Fqsysprt   O    F  132        PRINTER OFLIND(*INOF)
001200900604     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
001300900604     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)
001400900604     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)
001500900604     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)
001600121025     d CMD6            S             63    DIM(1) CTDATA PERRCD(1)
001601121026     d CMD7            S             64    DIM(1) CTDATA PERRCD(1)
001700941014     D CSBDS         E DS                  EXTNAME(TNCSB00F)
001800941014     D               E DS                  EXTNAME(TNCSV00F)
001900941014     D  CSVDS                 16    229
002000000000     D KPJBA         E DS
002100900604     D  LIBSYS                92    101
002200900604     D  LEGFIL               483    492
002300900604     D  MBRFIL               493    502
002400000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002500020208     d*
002600020306     D OG143         E DS
002700020306     D TIBS69        E DS                  EXTNAME(TIBS69DS)
002800020208     D CNACO         E DS                  EXTNAME(CNACO00F)
002900020208     D CNIND         E DS                  EXTNAME(CNIND00F)
003000020208     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003100020208     D FNCLS         E DS                  EXTNAME(FNCLS00F)
003101021014      *parametri per reperimento coda
003102021014     d finep           s              1
003103021014     d codpo           s              3s 0
003104021014     d tpcoda          s              1
003106021014     d libcoda         s             10
003107021014     d esito           s              1
003200020212     d outq            s             10
003201121025     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
003202121025     D ESITO_OK...
003203121025     D                 C                   0
003204121025     ***************************************************************************
003205121025     **
003206121025     ** Dichiarazione prototipi procedure esterne.
003207121025     **
003208121025     ***************************************************************************
003209121025      /DEFINE DFTACTGRP_YES
003210121025     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
003211121025      /UNDEFINE DFTACTGRP_YES
003212121025     ***************************************************************************
003213121025     **
003214121025     ** Definizione strutture dati.
003215121025     **
003216121025      **************************************************************************
003217121025     D tibsSocI0...
003218121025     D               E DS                  QUALIFIED
003219121025     D                                     INZ
003220121025     D tibsSocO0...
003221121025     D               E DS                  QUALIFIED
003222121025     D                                     INZ
003223121025     ***************************************************************************
003224121025     **
003225121025     ** Definizione variabili modulo/programma.
003226121025     **
003227121025     ***************************************************************************
003228121025     D prmRqsOpCode...
003229121025     D                 S             10A
003230121025     D prmRpyOpCode...
003231121025     D                 S             10A
003232121025     D prmRpyIdMsg...
003233121025     D                 S             10I 0
003234121025     D prmRqsFormato...
003235121025     D                 S             10A
003236121025     D prmRqsDataSize...
003237121025     D                 S             10I 0
003238121025     D prmRpyFormato...
003239121025     D                 S             10A
003240121025     D prmRpyDataSize...
003241121025     D                 S             10I 0
003400941014     C*--------------------------------------------------------------*
003500020205     c                   do        *hival
003600020205     C                   READ      FIcso00R
003700020205     c                   if        %eof(ficso00r)
003800020205     C                   leave
003900020205     c                   end
004000900604     C                   ADD       1             NUREC             7 0
004100020205     C** CONTRASSEGNO NON PRESENTE
004200020205     C     KCON          CHAIN     TNCSB03l
004300020205     c                   select
004400020205     C                   when      not %found(tncsb03l)
004500020205     c                   seton                                        01
004600020205     c                   exsr      stampa
004700020205     C** CONTRASSEGNO NON STATO 3
004800020205     C                   when      csbsta <> 3
004900020205     c                   seton                                        02
005000020205     c                   exsr      stampa
005100020205     C** CONTRASSEGNO OMESSO
005200020206     C* CREO STORICO VARIAZIONI C/A con causale CRSE
005300020206     C                   when      %found(tncsb03l) and csbsta = 3
005400910913     C                   EXSR      CSVWTR
005500020206     C* azzero dati trasmissione così mando il nuovo stato e FNCSB in
005600020206     c* filiale partenza per poter poi pulire la bolla
005700020206     c                   clear                   csbftr
005800020206     c                   z-add     0             csbdtr
005900020206     c* stato = 8
006000020205     C                   MOVEL     csoSTA        CSBSTA
006100941014     C                   UPDATE    TNCSB000
006200020205     C                   Endsl
006300020208     c* stampa lettera nell'outq di filiale
006400020208     c                   exsr      stalet
006500020208     C*
006600020205     C                   DELETE    FICSO000
006700020205     C                   enddo
006800900604     C* CHIUSURA E RIORGANIZZAZIONE MEMBRO TRASCODIFICATO
006900900604     C                   EXSR      CAEXB
007000900604     C* AGGIORNA ARCHIVIO DI LOG
007100900604     C     LEGFIL        CHAIN     CMRIC01L                           20
007200900604     C  N20              DO
007300900604     C                   Z-ADD     NUREC         RICNR1
007400900604     C                   MOVE      '1'           RICFL2                          AGG.TO
007500900604     C                   UPDATE    CMRIC000
007600900604     C                   MOVEL     RICPRR        LEGFIL
007700900604     C                   END
007800900604     C     STOP          TAG
007801121025     ** Chiudo il programma.
007802121025     C                   CALL      'TIBSSOCR'
007803121025     C                   PARM      'FINALIZE'    prmRqsOpCode
007804121025     C                   PARM                    prmRpyOpCode
007805121025     C                   PARM                    prmRpyIdMsg
007900000000     C                   SETON                                        LR
008000910913     C**---------------------------------------------------------
008100941014     C** ESEGUE WRITE TNCSV00F                                  -
008200910913     C**---------------------------------------------------------
008300910913     C     CSVWTR        BEGSR
008400020205     C                   MOVEL     csoCAV        CSVCAV
008500020205     C                   MOVE      csoDTV        CSVDTV
008600020205     C                   MOVE      csoORV        CSVORV
008700020205     C                   MOVEL     csoFEV        CSVFEV
008800910913     C                   MOVEL     CSBDS         CSVDS
008900020212     C                   MOVEL     CSOsta        CSVsta
009000941014     C                   WRITE     TNCSV000
009100910913     C                   ENDSR
009200020208     C**-------------------------------------------------------------**
009300020208     C** Stampa lettera per sede
009400020208     C**-------------------------------------------------------------**
009500020208     C     stalet        BEGSR
009600020208     c* contrassegno
009700020208     C                   MOVE      CSBLNP        SLNP
009800020208     C                   MOVE      CSBAAS        SAAS
009900020208     C                   MOVE      CSBNRS        SNRS
010000020208     C                   MOVE      CSBNSP        SNSP
010100020208     C                   MOVE      CSBMGS        WDUE              2 0
010200020208     C                   MOVEL     WDUE          SGMSP
010300020208     C                   MOVEL     CSBMGS        WDUE
010400020208     C                   MOVE      WDUE          SGMSP
010500020208     C                   MOVEL     csbvca        Sdiv
010600020208     C                   Z-ADD     csbCAS        SASS
010601121025     C                   MOVEL     csbvca        Sdiv2
010602121025     C                   Z-ADD     csbCAS        SASS2
011116121025     c                   eval      outq = 'P046A097S'
012900020208     c* destinatario spedizione
013000020208     c     kcon          chain     titas30c
013100020208     c                   if        %found(titas30c)
013200020208     C                   MOVEL     tasrsd        ragdo
013201121025     C                   MOVEL     tasrsd        ragdo2
013202121025     C                   MOVEL     tasind        inddo
013203121025     C                   MOVEL     tascad        caddo
013204121025     C                   MOVEL     taslod        loddo
013205121029     C                   MOVEL     tasprd        prddo
013206121029     C*                  MOVEL     tasnzd        nzddo
013300020208     c                   else
013400020208     C                   MOVEL     *ALL'?'       ragdo
013500020208     c                   end
013600020208     c* mittente spedizione
013700020208     C                   MOVE      csbCDI        §8899             4
013800020208     c                   if        §8899 = '8888' or §8899 = '9999'
013900020208     c     kcon          chain     tncsa01l
014000020208     c                   if        %found(tncsa01l)
014100020208     C                   MOVEL     csarsd        ragmo
014200020208     c                   else
014300020208     C                   MOVEL     *ALL'?'       ragmo
014400020208     c                   end
014500020208     c                   else
014600020208     C                   CLEAR                   TIBS69
014700020208     C                   CLEAR                   CNACO
014800020208     C                   cLEAR                   CNIND
014900020208     C                   CLEAR                   CNCLP
015000020208     C                   CLEAR                   FNCLS
015100020208     C                   MOVEL     ' '           I69TLA
015200020208     C                   MOVEL     KNSIF         I69SIF
015300020208     C                   Z-ADD     0             I69KCC
015400020208     C                   Z-ADD     csbcdi        I69KAC
015500020208     C                   Z-ADD     csbcdi        I69KIN
015600020208     C                   Z-ADD     0             I69KCP
015700020208     C                   CALL      'TIBS69R'
015800020208     C                   PARM                    TIBS69
015900020208     C                   PARM                    CNACO
016000020208     C                   PARM                    CNIND
016100020208     C                   PARM                    CNCLP
016200020208     C                   PARM                    FNCLS
016300020208     C                   IF        o69err = *BLANK
016400020208     C                   eval      ragmo = acorag
016500020208     c                   end
016600020208     c                   end
016700020213     c* apro il file di stampa
016701121025     c                   if        aperto = *blank
016800121025     c                   exsr      sropen
016801121025     c                   move      'X'           aperto            1
016802121025     c                   endif
016900020208     c*
016901121026     c     csblna        chain     azorg01l
016902121026     c                   if        %found(azorg01l)
016903121026     c                   move      orgde3        og143
016904121026     c                   endif
016905121026      *
016906121026     c                   if        §ogntw = 'FED' or
016907121026     c                             §ogntw = 'DPD' or
016908121026     c                             §ogntw = 'EEX'
016909121026     c                   write     tes01e
016910121026     c                   else
017000020208     c                   write     tes01
017001121026     c                   endif
017100121025     c*m                 close     fnlro2p
017200020208     C*
017300020208     c                   endsr
017400900604     C**---------------------------------------------------------
017500020212     C** apro il file di stampa                                 -
017600900604     C**---------------------------------------------------------
017700020212     C     sropen        BEGSR
017800020212     C*
017900121025     C                   Z-ADD     65            LUNG             15 5
018000020212     C** OVRPRTF sulla stampante del punto operativo di arrivo
018100020212     C                   MOVEl(p)  CMD6(1)       COMMAN
018200020212     c                   eval      %subst(comman: 31: 10) = outq
018300020212     C                   CALL      'QCMDEXC'
018400020212     C                   PARM                    COMMAN
018500020212     C                   PARM                    LUNG
018600020212     c                   open      fnlro2p
018700020212     C*
018701121026     C                   Z-ADD     65            LUNG             15 5
018702121026     C** OVRPRTF sulla stampante del punto operativo di arrivo
018703121026     C                   MOVEl(p)  CMD7(1)       COMMAN
018704121026     c                   eval      %subst(comman: 32: 10) = outq
018705121026     C                   CALL      'QCMDEXC'
018706121026     C                   PARM                    COMMAN
018707121026     C                   PARM                    LUNG
018708121026     c                   open      fnlro2pe
018709121026     C*
018800020212     c                   endsr
018900020212     C**---------------------------------------------------------
019000020212     C** ESEGUE QCAEXEC DI APERTURA SUL MEMBRO DI FICSO00R      -
019100020212     C**---------------------------------------------------------
019200020212     C     CAEXA         BEGSR
019300020205     C                   MOVE      ')'           VAR11            11
019400020205     C                   MOVEL     MBRFIL        VAR11
019500020205     C                   MOVE      '))'          VAR12            12
019600020205     C                   MOVEL     MBRFIL        VAR12
019700900604     C                   Z-ADD     55            LUNG             15 5
019800900604     C*
019900900604     C                   SETOFF                                       20
020000020205     C                   MOVE      VAR11         CMD1
020100020205     C                   MOVEl(p)  CMD1(1)       COMMAN           80
020200900604     C                   CALL      'QCMDEXC'                            20
020300900604     C                   PARM                    COMMAN
020400900604     C                   PARM                    LUNG
020500900604     C   20              GOTO      STOP
020600900604     C** CONTROLLO SE ESISTE IL MEMBRO
020700900604     C** SE NON ESISTE CHIUDO IL PROGRAMMA
020800900604     C                   SETOFF                                       20
020900020205     C                   MOVE      VAR12         CMD2
021000020205     C                   MOVEl(p)  CMD2(1)       COMMAN
021100900604     C                   CALL      'QCMDEXC'                            20
021200900604     C                   PARM                    COMMAN
021300900604     C                   PARM                    LUNG
021400900604     C   20              GOTO      STOP
021500900604     C** ALLOCO MEMBRO
021600900604     C** SE NON RIESCE CHIUDO IL PROGRAMMA
021700900604     C                   SETOFF                                       20
021800020205     C                   MOVE      VAR11         CMD3
021900020205     C                   MOVEl(p)  CMD3(1)       COMMAN
022000900604     C                   CALL      'QCMDEXC'                            20
022100900604     C                   PARM                    COMMAN
022200900604     C                   PARM                    LUNG
022300900604     C   20              GOTO      STOP
022400900604     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
022500900604     C** SE NON RIESCE CHIUDO IL PROGRAMMA
022600020205     C                   OPEN      FICSO00R
022700900604     C                   ENDSR
022800900604     C**---------------------------------------------------------
022900990720     C** ESEGUE QCAEXEC DI CHIUSURA SUL MEMBRO DI FIGIC00R      -
023000900604     C**---------------------------------------------------------
023100900604     C     CAEXB         BEGSR
023200900604     C                   MOVE      '))'          VAR12            12
023300900604     C                   MOVEL     MBRFIL        VAR12
023400900604     C                   Z-ADD     55            LUNG             15 5
023500900604     C*
023600020205     C                   CLOSE     FICSO00R
023700020205     C** DISALLOCA IL MEMBRO AGG.TO
023800900604     C                   MOVE      VAR12         CMD5
023900020205     C                   MOVEl(p)  CMD5(1)       COMMAN
024000900604     C                   CALL      'QCMDEXC'
024100900604     C                   PARM                    COMMAN
024200900604     C                   PARM                    LUNG
024300900604     C                   ENDSR
024400941014     C**---------------------------------------------------------
024500020205     C** Stampa errori
024600941014     C**---------------------------------------------------------
024700020205     C     stampa        BEGSR
024800020205     c*
024900020205     c   of              except    test
025000020205     c                   setoff                                       of
025100020205     c                   except    det
025200020205     c   01              except    err1
025300020205     c   02              except    err2
025400020205     c                   setoff                                       0102
025500020205     c*
025600020205     C                   ENDSR
025601121025     C**************************************************************************
025602121025     C* decodifica la società
025603121025     C************************************************************
025604121025     C     Decod_societa begSR
025605121025     ** Inizializzo il programma.
025606121025     C                   CALL      'TIBSSOCR'
025607121025     C                   PARM      'INIT'        prmRqsOpCode
025608121025     C                   PARM                    prmRpyOpCode
025609121025     C                   PARM                    prmRpyIdMsg
025610121025      *
025611121025     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
025612121025     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
025613121025     c* forzo società 201
025614121025     C                   EVAL      tibsSocI0.idSocieta ='201'
025615121025     C
025616121025     C                   CALL      'TIBSSOCR'
025617121025     C                   PARM      'GETANAGRAF'  prmRqsOpCode
025618121025     C                   PARM      *BLANK        prmRpyOpCode
025619121025     C                   PARM      *ZERO         prmRpyIdMsg
025620121025     C                   PARM      'TIBSSOCI0'   prmRqsFormato
025621121025     C                   PARM                    tibsSocI0
025622121025     C                   PARM                    prmRqsDataSize
025623121025     C                   PARM      'TIBSSOCO0'   prmRpyFormato
025624121025     C                   PARM                    tibsSocO0
025625121025     C                   PARM                    prmRpyDataSize
025626121025     c                   if         PRMRPYIDMSG >= 0
025627121025     c                              and TIBSSOCO0.IDSOCIETA <> *blank
025628121029     c*                  eval       brtdes  = tibsSocO0.RAGSOCIALE
025629121029     c*                  eval       brttel  = tibsSocO0.SEDLEGTEL
025630121029     c*                  eval       brtfax  = tibsSocO0.SEDLEGFAX
025631121025     c                   else
025632121029     c*                  eval       brtdes  = *all'?'
025633121025     c                   end
025634121025      *
025635121025     c                   endsr
025700020205     C**---------------------------------------------------------
025800020205     C** OPERAZIONI INIZIALI
025900020205     C**---------------------------------------------------------
026000020205     C     *INZSR        BEGSR
026100941014     C     *ENTRY        PLIST
026200941014     C                   PARM                    KPJBA
026201121025     c                   move      *date         date
026300941014     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
026400941014     C* ASSUMO IL PRIMO MEMBRO DEL FILE
026500941014     C     MBRFIL        IFEQ      *BLANK
026600941014     C                   MOVEL     '*FIRST'      MBRFIL
026700941014     C                   END
026800941014     C                   Z-ADD     1             CODUT
026900941014     C                   CALL      'XPARUT'
027000941014     C                   PARM                    UTEDSE
027100941014     C     CODUT         CABEQ     -1            STOP
027200000128     C                   MOVEL     RAGUT         RSUT             20
027300941014     C*---------------------------------------------------------------*
027400941014     C* APERTURA MEMBRO DA TRASCODIFICARE
027500941014     C                   EXSR      CAEXA
027600941014     C*---------------------------------------------------------------*
027700941014     C* KLIST
027800941014     C     KCON          KLIST
027900020205     C                   KFLD                    csoAAS
028000020205     C                   KFLD                    csoLNP
028100020205     C                   KFLD                    csoNRS
028200020205     C                   KFLD                    csoNSP
028300941014     C*
028400020205     c                   seton                                        of
028500941014     C                   ENDSR
028600020212     Oqsysprt   E            TEST           2 02
028700000000     O                       RSUT
028800020205     O                                         + 15 'RICEZIONE IN SEDE VARIAZIO'
028900020205     O                                         +  0 'NE CRSE'
029000020205     O                       udate             +  1
029100020205     O                                         +  1 'FNLRO2R'
029200000000     O                                         +  5 'PAG.'
029300000000     O                       PAGE          Z   +  1
029400020205     O          E            TEST           2
029500900615     O                                              'FILE:'
029600900615     O                       LEGFIL            +  1
029700900625     O                                         +  2 'MEMBRO:'
029800900615     O                       MBRFIL            +  1
029900020205     O          E            TEST           2
030000900615     O                                              'L.P.'
030100920403     O                                         +  1 'SER.'
030200900615     O                                         +  1 'SPEDIZ.'
030300941014     O                                         +  1 'ANNO'
030400900615     O                                         +  1 'L.A.'
030500900615     O                                         + 10 'NOTE'
030600020205     O          E            DET         1
030700020212     O                       csoLNP            +  0
030800020212     O                       csoNRS        Z   +  2
030900020212     O                       csoNSP        Z   +  3
031000020212     O                       csoAAS            +  1
031100020212     O                       csoLNA            +  1
031200020205     O          E            ERR1        0
031300020212     O                                         + 37 'C/ASSEGNO NON TROVATO'
031400020205     O          E            ERR2        0
031500020212     O                                         + 37 'C/ASSEGNO NON PIU'' IN STA'
031600020205     O                                         +  0 'TO 3'
031700990720**         CMD1
031800020205CHKOBJ OBJ(FICSO00R) OBJTYPE(*FILE)     MBR(M234567890)
031900900604**         CMD2
032000020205ALCOBJ OBJ((FICSO00R *FILE *EXCL           M234567890))
032100900604**         CMD3
032200020205OVRDBF FILE(FICSO00R)                   MBR(M234567890)
032300900604**         CMD5
032400020205DLCOBJ OBJ((FICSO00R *FILE *EXCL           M234567890))
032500020212**         CMD6
032600121025OVRPRTF    FILE(FNLRO2P) OUTQ(xxxxxxxxxx) SAVE(*YES) hold(*yes)
032700121026**         CMD7
032800121026OVRPRTF    FILE(FNLRO2Pe) OUTQ(xxxxxxxxxx) SAVE(*YES) hold(*yes)
